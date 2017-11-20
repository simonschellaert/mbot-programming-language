import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import Data.Foldable
import qualified Text.Read as Read

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
ident = do c <- lower
           cs <- many alphanum
           return (c:cs)

-- A parser that consumes a natural number
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

-- A parser that consumes a boolean (i.e. a string `true` or `false`)
bool :: Parser Bool
bool = do string "true"
          return True
       <|>
       do string "false"
          return False

-- A parser that applies the three parsers `open`, `p` and `close` one after another. Only the
-- results of `p` are kept and returned. This is useful to take care of brackets, hence the name.
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

-- A parser that applies each given parser and returns its associated value if it succeeds
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
natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: Parser String
identifier = token ident

boolean :: Parser Bool
boolean = token bool

newline :: Parser ()
newline = void (token (char '\n'))

indent :: Parser ()
indent = newline >> symbol "{" >> newline

dedent :: Parser ()
dedent = newline >> symbol "}" >> return ()

data AExpr = AConst Int
           | AVar String
           | AExpr :+: AExpr
           | AExpr :-: AExpr
           | AExpr :*: AExpr
           | AExpr :/: AExpr
           deriving (Show)

-- A parser for arithmetic expressions
aExpression :: Parser AExpr
aExpression = aTerm `chainl1` ops [(symbol "+", (:+:)), (symbol "-", (:-:))]

aTerm :: Parser AExpr
aTerm = aFactor `chainl1` ops [(symbol "*", (:*:)), (symbol "/", (:/:))]

aFactor :: Parser AExpr
aFactor = fmap AConst integer
          <|> fmap AVar identifier
          <|> brackets (symbol "(") aExpression (symbol ")")


data BExpr = BConst Bool
           | Not BExpr
           | BExpr :&: BExpr
           | BExpr :|: BExpr
           | AExpr :<: AExpr
           | AExpr :=: AExpr
           | AExpr :>: AExpr
           deriving (Show)


bExpression :: Parser BExpr
bExpression = bTerm `chainl1` ops [(symbol "||", (:|:))]

bTerm :: Parser BExpr
bTerm = bFactor `chainl1` ops [(symbol "&&", (:&:))]

bFactor :: Parser BExpr
bFactor = fmap BConst boolean
          <|> brackets (symbol "(") bExpression (symbol ")")
          <|> fmap   Not   (symbol "!" >> bFactor)
          <|> liftM2 (:<:) aExpression (symbol "<"  >> aExpression)
          <|> liftM2 (:=:) aExpression (symbol "==" >> aExpression)
          <|> liftM2 (:>:) aExpression (symbol ">"  >> aExpression)

data Stmt = Assign String AExpr
          | Seq [Stmt]
          | If BExpr Stmt
          | While BExpr Stmt
          | Skip
          deriving (Show)

-- A parser for a sequence of statements separated by one or more empty lines
statement :: Parser Stmt
statement = fmap Seq (singleStatement `sepby1` (char '\n'))
            where singleStatement = do spaces <|> return ()
                                       assignStatement <|> ifStatement <|> skipStatement

assignStatement :: Parser Stmt
assignStatement = liftM2 Assign identifier (symbol "=" >> aExpression)

ifStatement :: Parser Stmt
ifStatement = do symbol "if"
                 cond <- bExpression
                 indent
                 body <- statement
                 dedent
                 return (If cond body)

-- A parser for comment statements. That is, it consumes '//' and then consumes all remaining
-- characters until the end of the line.
skipStatement :: Parser Stmt
skipStatement = do symbol "//"
                   first . many $ sat (/='\n')
                   return Skip


main = do putStrLn "Please type an arithmetic expression involving +, -, *, /, integers or variablesss"
          inp <- readFile "demo.txt"
          let inp' = preprocess inp
          putStrLn inp'
          let out = parse statement inp'
          mapM_ print out
          unless (null out) (print (head out))


type Name = String
type Env = Map.Map Name Int

-- TODO: Use some fancy monad transformers to keep track of the environment and errors
type Eval a = ReaderT Int (StateT Env IO) a

evalB :: BExpr -> Eval Bool
evalB (BConst b)  = return b
evalB (Not b)     = fmap not (evalB b)
evalB (b1 :&: b2) = liftM2 (&&) (evalB b1) (evalB b2)
evalB (b1 :|: b2) = liftM2 (||) (evalB b1) (evalB b2)
evalB (a1 :<: a2) = liftM2 (<)  (evalA a1) (evalA a2)
evalB (a1 :=: a2) = liftM2 (==) (evalA a1) (evalA a2)
evalB (a1 :>: a2) = liftM2 (>)  (evalA a1) (evalA a2)

evalA :: AExpr -> Eval Int
evalA (AConst a)  = return a
evalA (AVar a)    = gets (fromJust . (Map.lookup a))
evalA (a1 :+: a2) = liftM2 (+) (evalA a1) (evalA a2)
evalA (a1 :-: a2) = liftM2 (-) (evalA a1) (evalA a2)
evalA (a1 :*: a2) = liftM2 (*) (evalA a1) (evalA a2)
evalA (a1 :/: a2) = liftM2 div (evalA a1) (evalA a2)


--          let inp' = unlines (evalState (preprocess (lines inp ++ [""])) [0])


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
