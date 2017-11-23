import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Foldable
import qualified Text.Read as Read
import qualified System.Posix.Unistd as Unistd

-- Create the Parser monad we all know and love
newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Monad Parser where
    return x = Parser $ \inp -> [(x, inp)]
    x >>= f  = Parser $ \inp -> concat [parse (f x') inp' | (x', inp') <- parse x inp]

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Parser where
    mzero       = Parser $ const []
    f `mplus` g = Parser $ \inp -> parse f inp ++ parse g inp

-- A parser that consumes a single character if the input is non-empty and fails otherwise
item :: Parser Char
item = Parser $ \inp -> case inp of
                          []     -> []
                          (x:xs) -> [(x, xs)]

-- A parser that consumes a single character that satisfies the given predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           guard (p c)
           return c

-- Various parsers that consume a single character of the specified type
digit    = sat isDigit
lower    = sat isLower
upper    = sat isUpper
letter   = sat isAlpha
alphanum = sat isAlphaNum
char x   = sat (==x)

-- A parser that consumes the specified string
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- A parser that consumes an identifier (i.e. an alphanumeric string starting with a lowercase letter)
ident = do c <- letter
           cs <- many alphanum
           return (c:cs)

-- A parser that consumes a natural number
-- TODO: Fix this ugly function
nat :: Parser Int
nat = do cs <- many digit
         case Read.readMaybe cs :: Maybe Int of
             Nothing -> empty
             Just x  -> return x

-- A parser that consumes an integer
int :: Parser Int
int = do char '-'
         fmap negate nat
      <|> nat

-- A parser that applies the three parsers `open`, `p` and `close` one after another. Only the
-- results of `p` are kept and returned. This is useful to take care of brackets, hence the name.
-- TODO: Only used for ( and ) so make less generic
brackets :: Parser a -> Parser b -> Parser c -> Parser b
brackets open p close = do open
                           x <- p
                           close
                           return x

-- A parser that recognizes non-empty sequences of `p` where instances of `p` are separated by `sep`
sepby1   :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do x <- p
                    xs <- many (sep >> p)
                    return (x:xs)

-- A parser that consumes strings produced by grammar 'E -> E | E `op` p'
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do x <- p
                    fys <- many (do f <- op
                                    y <- p
                                    return (f, y))
                    return (foldl (\x (f,y) -> f x y) x fys)

-- 
first :: Parser a -> Parser a
first p = Parser $ \inp -> case parse p inp of
                             [] -> []
                             (x:xs) -> [x]

-- A parser that applies each of the given parsers and returns its associated value iff it succeeds.
ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (<|>) [p >> return op | (p,op) <- xs]

-- A parser that consumes whitespace, but not newlines. Note that this parser can fail if there's no
-- whitespace to consume.
spaces :: Parser ()
spaces = first . void $ some (sat isWhite)
         where isWhite c = isSpace c && c /= '\n' 

-- A parser that applies the given parser and then tries to consume any remaining whitespace till the end of the line.
token :: Parser a -> Parser a
token p = first (do x <- p
                    spaces <|> return ()
                    return x)

-- Various parser that consuming any remaining whitespace till the end of the line after consuming the specified
-- type of input. These parsers will prove very useful for implementing higher-level parsers.
integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: Parser String
identifier = token ident

boolean :: Parser Bool
boolean = (trueSymbol  >> return True)
      <|> (falseSymbol >> return False)

newline :: Parser ()
newline = void (token (char '\n'))

indent :: Parser ()
indent = newline >> indentSymbol >> newline

dedent :: Parser ()
dedent = newline >> dedentSymbol >> return ()

block :: Parser Stmt
block = do indent
           body <- statementSeq
           dedent
           return body

data AExpr = AConst Int
           | AVar Name
           | ASensor Sensor
           | AExpr :+: AExpr
           | AExpr :-: AExpr
           | AExpr :*: AExpr
           | AExpr :/: AExpr
           deriving (Show, Eq)

-- A parser for arithmetic expressions
aExpression :: Parser AExpr
aExpression = aTerm `chainl1` ops [(addSymbol, (:+:)), (subtractSymbol, (:-:))]

aTerm :: Parser AExpr
aTerm = aFactor `chainl1` ops [(multiplySymbol, (:*:)), (divideSymbol, (:/:))]

aFactor :: Parser AExpr
aFactor = aConstant
      <|> aSensor
      <|> fmap AVar identifier
      <|> brackets openParSymbol aExpression closeParSymbol

aConstant :: Parser AExpr
aConstant = fmap AConst (integer
                    <|> (zeroSymbol  >> return 0)
                    <|> (oneSymbol   >> return 1)
                    <|> (twoSymbol   >> return 2)
                    <|> (threeSymbol >> return 3))


aSensor :: Parser AExpr
aSensor = fmap ASensor ((lineSymbol     >> return Line)
                    <|> (distanceSymbol >> return Distance))


data BExpr = BConst Bool
           | Not BExpr
           | BExpr :&: BExpr
           | BExpr :|: BExpr
           | AExpr :<: AExpr
           | AExpr :=: AExpr
           | AExpr :>: AExpr
           deriving (Show)


bExpression :: Parser BExpr
bExpression = bTerm `chainl1` ops [(orSymbol, (:|:))]

bTerm :: Parser BExpr
bTerm = bFactor `chainl1` ops [(andSymbol, (:&:))]

bFactor :: Parser BExpr
bFactor = fmap BConst boolean
          <|> brackets openParSymbol bExpression closeParSymbol
          <|> fmap   Not   (negateSymbol >> bFactor)
          <|> liftM2 (:<:) aExpression (ltSymbol  >> aExpression)
          <|> liftM2 (:=:) aExpression (eqSymbol  >> aExpression)
          <|> liftM2 (:>:) aExpression (gtSymbol  >> aExpression)

data Stmt = Assign String AExpr
          | Seq [Stmt]
          | If [(BExpr, Stmt)]
          | While BExpr Stmt
          | Exec Command
          | Skip
          deriving (Show)


data Sensor = Line | Distance deriving (Show, Eq)
data Direction = Left | Right | Up | Down deriving (Show, Eq)
data Flank = LeftFlank | RightFlank deriving (Show, Eq)
data Duration = VeryShort | Short | Medium | Long | VeryLong | Exact AExpr deriving (Show, Eq)

data Command = Drive Direction
             | Sleep Duration
             | Print String
             | Light Flank AExpr AExpr AExpr
             deriving (Show)



-- A parser for a sequence of statements. A sequence of statements consists of one or more statements separated by a
-- newline character.
statementSeq :: Parser Stmt
statementSeq = fmap Seq (singleStatement `sepby1` (char '\n'))

-- A parser for a single statement. Note that this parser consumes any leading whitespace before attempting to parse
-- the actual statement.
singleStatement :: Parser Stmt
singleStatement = do spaces <|> return ()
                     assignStatement <|> ifStatement <|> skipStatement <|> whileStatement <|> cmdStatement

assignStatement :: Parser Stmt
assignStatement = liftM2 Assign identifier (assignSymbol >> aExpression)

ifStatement :: Parser Stmt
ifStatement = do ifSymbol
                 cond <- bExpression
                 body <- block
                 elifClauses <- first (many elifClause)
                 elseClause  <- (newline >> elseSymbol >> block) <|> (return Skip)
                 return . If $ [(cond, body)] ++ elifClauses ++ [(BConst True, elseClause)]

              where elifClause :: Parser (BExpr, Stmt)
                    elifClause = do newline
                                    elifSymbol
                                    cond <- bExpression
                                    body <- block
                                    return (cond, body)

whileStatement :: Parser Stmt
whileStatement = do whileSymbol
                    cond <- bExpression
                    body <- block
                    return (While cond body)




cmdStatement :: Parser Stmt
cmdStatement = driveCmdStatement <|> sleepCmdStatement <|> lightCmdStatement <|> printCmdStatement

driveCmdStatement :: Parser Stmt
driveCmdStatement = do driveSymbol
                       dir <- (leftSymbol  >> return Main.Left)
                          <|> (rightSymbol >> return Main.Right)
                          <|> (upSymbol    >> return Main.Up)
                          <|> (downSymbol  >> return Main.Down)
                       return . Exec . Drive $ dir

sleepCmdStatement :: Parser Stmt
sleepCmdStatement = do sleepSymbol
                       duration <- (veryShortSymbol >> return VeryShort)
                               <|> (shortSymbol     >> return Short)
                               <|> (mediumSymbol    >> return Medium)
                               <|> (longSymbol      >> return Long)
                               <|> (veryLongSymbol  >> return VeryLong)
                               <|> (fmap Exact aExpression)
                       return . Exec . Sleep $ duration

lightCmdStatement :: Parser Stmt
lightCmdStatement = do lightSymbol
                       flank <- (leftFlankSymbol  >> return LeftFlank)
                            <|> (rightFlankSymbol >> return RightFlank)
                       cmd <- liftM3 (Light flank) aExpression aExpression aExpression
                       return . Exec $ cmd

printCmdStatement :: Parser Stmt
printCmdStatement = do printSymbol
                       text <- first . many $ sat (/='\n')
                       return . Exec . Print $ text


-- A parser for comment statements. That is, it consumes '//' and then consumes all remaining
-- characters until the end of the line.
skipStatement :: Parser Stmt
skipStatement = do skipSymbol
                   first . many $ sat (/='\n')
                   return Skip


main = do putStrLn "Please type an arithmetic expression involving +, -, *, /, integers or variablesss"
          inp <- readFile "demo.txt"
          let inp' = preprocess inp
          putStrLn inp'
          let out = parse statementSeq inp'
          print out
          unless (null out) (do let prog = fst (head out)
                                runEval logDevice (eval prog)
                                return ())
          -- runEval logDevice (eval out)
          --let out = eval (parse statement inp')
          --mapM_ print out
          --




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
                             where directions = [(Main.Left, (80, 0)),  (Main.Right, (0, 80)),
                                                 (Main.Up,   (80, 80)), (Main.Down,  (-80, -80))]

-- Evaluates a statement calling the light command. We first inspect the flank to know which of the 2 light indices to
-- send to the device, and then evaluate each of the three arithmetic expressions passed as arguments before sending
-- a command to the device with those values.
eval (Exec (Light flank r g b)) = do let light = if flank == LeftFlank then 1 else 2
                                     cmd <- asks setRGB
                                     liftIO =<< (liftM4 cmd (return light) (evalA r) (evalA g) (evalA b))

-- TODO: Remove this after debugging.
eval (Exec (Print txt)) = liftIO. putStrLn $ txt


preprocess :: String -> String
preprocess = unlines . (flip evalState [0]) . addMarkers . (++ [""]) . filter (not . all isSpace) . lines

-- TODO: Empty lines should pop the stack
addMarkers :: [String] -> State [Int] [String]
addMarkers []     = return []
addMarkers (l:ls) = do indents <- get
                       let cur = length . takeWhile isSpace $ l
                       l' <- case compare cur (head indents) of
                               GT -> do modify (cur:)
                                        return ("{\n" ++ l)
                               LT -> do let indents' = dropWhile (>cur) indents
                                        put indents'
                                        let diff = length indents - length indents'
                                        return ((concat (replicate diff "}\n")) ++ l)
                               EQ -> return l
                       liftM2 (:) (return l') (addMarkers ls)


prompt s = do
    putStr s
    line <- getLine
    return (read line)

logDevice = Device {
    sleep        = \x -> putStrLn ("Sleeping for " ++ show x ++ " milliseconds") >> Unistd.usleep (x * 1000),
    setRGB       = \l r g b -> putStrLn ("Setting light " ++ show l ++ " to " ++ show r ++ ", " ++ show g ++ ", " ++ show b),
    setMotor     = \l r -> putStrLn ("Setting motor speed to " ++ show l ++ ", " ++ show r),
    readDistance = prompt "Enter the measurement of the distance sensor ",
    readLine     = prompt "Enter the measurement of the line sensor (0, 1, 2 or 3)"
}




-- In order not to clutter our code with Emoji, we aggregate all the tokens used in our language here. Note that
-- `symbol` is a function that maps a string to a parser that consumes that string and any remaining whitespace.
skipSymbol       = symbol "💭"
whileSymbol      = symbol "🔁"
ifSymbol         = symbol "❓"
elifSymbol       = symbol "⁉️"
elseSymbol       = symbol "❗️"
sleepSymbol      = symbol "😴"
veryShortSymbol  = symbol "🕑"
shortSymbol      = symbol "🕔"
mediumSymbol     = symbol "🕧"
longSymbol       = symbol "🕖"
veryLongSymbol   = symbol "🕙"
upSymbol         = symbol "⬆️"
downSymbol       = symbol "⬇️"
leftSymbol       = symbol "⬅️"
rightSymbol      = symbol "➡️"
driveSymbol      = symbol "💨"
trueSymbol       = symbol "👍"
falseSymbol      = symbol "👎"
leftFlankSymbol  = symbol "👈"
rightFlankSymbol = symbol "👉"
distanceSymbol   = symbol "📏"
lineSymbol       = symbol "🔭"
assignSymbol     = symbol "⏪"
lightSymbol      = symbol "🚨"
printSymbol      = symbol "🖋"
zeroSymbol       = symbol "🌑"
oneSymbol        = symbol "🌓"
twoSymbol        = symbol "🌗"
threeSymbol      = symbol "🌕"
ltSymbol         = symbol "<"
eqSymbol         = symbol "=="
gtSymbol         = symbol ">"
addSymbol        = symbol "+"
subtractSymbol   = symbol "-"
multiplySymbol   = symbol "*"
divideSymbol     = symbol "/"
andSymbol        = symbol "&&"
orSymbol         = symbol "||"
negateSymbol     = symbol "!"
indentSymbol     = symbol "{"
dedentSymbol     = symbol "}"
openParSymbol    = symbol "("
closeParSymbol   = symbol ")"



-- TODO: Implement music statement
