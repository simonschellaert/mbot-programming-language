module Evaluator where -- TODO: Limit exports

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Foldable
import Prelude hiding (Left, Right)

data AExpr = AConst Int
           | AVar Name
           | ASensor Sensor
           | AExpr :+: AExpr
           | AExpr :-: AExpr
           | AExpr :*: AExpr
           | AExpr :/: AExpr
           deriving (Show, Eq)

data BExpr = BConst Bool
           | Not BExpr
           | BExpr :&: BExpr
           | BExpr :|: BExpr
           | AExpr :<: AExpr
           | AExpr :=: AExpr
           | AExpr :>: AExpr
           deriving (Show)

data Stmt  = Assign String AExpr
           | Seq [Stmt]
           | If [(BExpr, Stmt)]
           | While BExpr Stmt
           | Exec Command
           | Skip
           deriving (Show)

data Command = Drive Direction
             | Sleep Duration
             | Light Flank AExpr AExpr AExpr
             | Print String -- TODO: Delete this
             deriving (Show)

data Sensor    = Line | Distance deriving (Show, Eq)
data Direction = Left | Right | Up | Down deriving (Show, Eq)
data Duration  = VeryShort | Short | Medium | Long | VeryLong | Exact AExpr deriving (Show, Eq)
data Flank     = LeftFlank | RightFlank deriving (Show, Eq)

data Device = Device {
    sleep        :: Int -> IO (),
    setRGB       :: Int -> Int -> Int -> Int -> IO (),
    setMotor     :: Int -> Int -> IO (),
    readDistance :: IO Int,
    readLine     :: IO Int
}

-- The environment used in the evaluator consists of a map from variable names to their corresponding values. This map
-- is passed around using the state monad transformer and can thus be modified.
type Name = String
type Mem  = Map.Map Name Int

-- Next to the mutable state described above, a immutable `Device` is also carried around in the evaluator. This device
-- is the device that commands are sent to. This lets us easily swap the physical Mbot for the simulator or vice versa.
type Eval a = ReaderT Device (StateT Mem IO) a

-- This function actually executed the specified evaluator with the specified type. Its result is an IO-action that
-- executes the program.
runEval :: Device -> Eval () -> IO ()
runEval device evaluator  = evalStateT (runReaderT evaluator device) Map.empty

-- An evaluator for boolean expressions. The operators is our language map nicely to operators built into Haskell.
evalB :: BExpr -> Eval Bool
evalB (BConst b)  = return b
evalB (Not b)     = fmap not (evalB b)
evalB (b1 :&: b2) = liftM2 (&&) (evalB b1) (evalB b2)
evalB (b1 :|: b2) = liftM2 (||) (evalB b1) (evalB b2)
evalB (a1 :<: a2) = liftM2 (<)  (evalA a1) (evalA a2)
evalB (a1 :=: a2) = liftM2 (==) (evalA a1) (evalA a2)
evalB (a1 :>: a2) = liftM2 (>)  (evalA a1) (evalA a2)

-- An evaluator for arithmetic expressions. The operators again map nicely to those built into Haskell.
evalA :: AExpr -> Eval Int
evalA (AConst a)         = return a
evalA (AVar a)           = gets (fromJust . (Map.lookup a)) -- Lookup the value of variable in the environment
evalA (ASensor Line)     = asks readLine >>= liftIO         -- Return the value of executing readLine on the device
evalA (ASensor distance) = asks readDistance >>= liftIO     -- Return the value of executing readDistance on the device
evalA (a1 :+: a2)        = liftM2 (+) (evalA a1) (evalA a2)
evalA (a1 :-: a2)        = liftM2 (-) (evalA a1) (evalA a2)
evalA (a1 :*: a2)        = liftM2 (*) (evalA a1) (evalA a2)
evalA (a1 :/: a2)        = liftM2 div (evalA a1) (evalA a2)

-- An evaluator for statements. Note that this evaluator doesn't return anything since the sole purpose of evaluating
-- a statement is its side effects.
eval :: Stmt -> Eval ()

-- Evaluates a sequence of statement by evaluating each statement in turn.
eval (Seq stmts)     = forM_ stmts eval

-- Evaluates an assignment statement by first evaluating the arithmetic expression on the right and then inserting the
-- value under the corresponding name in the map of variables.
eval (Assign name val) = do val' <- evalA val
                            modify (Map.insert name val')

-- Evaluates an if statement by first evaluating the conditions of each branch, and then evaluating the first branch
-- whose condition did evaluate to true or nothing if the condition of all branches is false.
-- Note that an eventual else branch always appears last with `BConst True` as its condition.
eval (If branches) = do conditions <- mapM (evalB . fst) branches
                        case (findIndex id conditions) of
                            Just i   -> eval . snd . (!!i) $ branches
                            Nothing  -> return ()

-- Evaluates a while statement by evaluating the condition, and iff the condition is true, evaluating the body and then
-- restarting this procedure.
eval (While e stmt) = do cond <- evalB e
                         when cond ((eval stmt) >> (eval (While e stmt)))

-- Evaluates a skip statement by doing nothing. A skip statement is a comment in the source code so it shouldn't do
-- anything.
eval Skip = return ()

-- Evaluates a statement calling the sleep command. Note that the argument of the sleep command is either an arithmetic
-- expression, in which case the expression is evaluated to determine its value or a built-in constant, in which case
-- the precise sleep duration is looked up in the association list `durations`.
eval (Exec (Sleep dur)) = do millis <- (case dur of
                                          Exact e  -> evalA e
                                          constant -> return . fromJust . (flip Data.List.lookup durations) $ constant)
                             asks sleep >>= liftIO . ($millis)
                             where durations = [(VeryShort, 100), (Short, 250), (Medium, 500),
                                                (Long, 1000), (VeryLong, 2000)]


-- Evaluates a statement calling the drive command. We first lookup the speed of both motors based on the direction and
-- then send a command to the device with these speeds.
eval (Exec (Drive dir)) = do let speed = fromJust . (flip Data.List.lookup directions) $ dir
                             asks setMotor >>= (liftIO . ($speed) . uncurry)
                             where directions = [(Left, (80, 0)),  (Right, (0, 80)),
                                                 (Up,   (80, 80)), (Down,  (-80, -80))]

-- Evaluates a statement calling the light command. We first inspect the flank to know which of the 2 light indices to
-- send to the device, and then evaluate each of the three arithmetic expressions passed as arguments before sending
-- a command to the device with those values.
eval (Exec (Light flank r g b)) = do let light = if flank == LeftFlank then 1 else 2
                                     cmd <- asks setRGB
                                     liftIO =<< (liftM4 cmd (return light) (evalA r) (evalA g) (evalA b))

-- TODO: Remove this after debugging.
eval (Exec (Print txt)) = liftIO. putStrLn $ txt
