import Control.Applicative
import Control.Monad
import Data.Char
import Text.Read

-- Create the Parser monad
newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
    fmap f p = Parser $ map (\(x,xs) -> (f x, xs)) . parse p

instance Applicative Parser where
    pure x   = Parser $ \inp -> [(x, inp)]
    f <*> g  = Parser $ \inp -> concat [parse (fmap f' g) inp' | (f', inp') <- parse f inp]

instance Alternative Parser where
    empty    = Parser $ const []
    f <|> g  = Parser $ \inp -> parse f inp ++ parse g inp

instance Monad Parser where
    return x = Parser $ \inp -> [(x, inp)]
    x >>= f  = Parser $ \inp -> concat [parse (f x') inp' | (x', inp') <- parse x inp]

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
         case readMaybe cs :: Maybe Int of
             Nothing -> empty
             Just x  -> return x

-- A parser that consumes an integer
int :: Parser Int
int = do char '-'
         fmap negate nat
      <|> nat

-- A parser that applies the three parsers `open`, `p` and `close` one after another. Only the
-- results of `p` are kept and returned. This is useful to take care of brackets, hence the name.
brackets :: Parser a -> Parser b -> Parser c -> Parser b
brackets open p close = do open
                           x <- p
                           close
                           return x

-- A parser that consumes strings produced by grammar 'E -> E | E `op` p'
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do x <- p
                    fys <- many (do f <- op
                                    y <- p
                                    return (f, y))
                    return (foldl (\x (f,y) -> f x y) x fys)

-- A parser that applies each given parser and returns its associated value if it succeeds
ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (<|>) [p >> return op | (p,op) <- xs]

-- A parser that consumes whitespace
spaces :: Parser ()
spaces = do some (sat isSpace)
            return ()

-- A parser that consumes a single-line comment
comment :: Parser ()
comment = do string "//"
             many (sat (/='\n'))
             return ()

-- A parser that repeatedly consumes comments and whitespace
junk :: Parser ()
junk = do many (spaces <|> comment)
          return ()

-- A parser that applies the given parser and then consumes any remaining junk
token :: Parser a -> Parser a
token p = do x <- p
             junk
             return x

-- Various parser that consuming any remaining whitespace or comments after consuming the specified
-- type of input. These parsers will prove the most useful for implementing higher-level parsers.
natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: Parser String
identifier = token ident



data AExpr = AConst Int
           | AVar String
           | Neg AExpr
           | Sum AExpr AExpr
           | Sub AExpr AExpr
           | Mul AExpr AExpr
           | Div AExpr AExpr
           deriving (Show)

-- A parser for arithmetic expressions
aExpression :: Parser AExpr
aExpression = aTerm `chainl1` ops [(symbol "+", Sum), (symbol "-", Sub)]

aTerm :: Parser AExpr
aTerm = aFactor `chainl1` ops [(symbol "*", Mul), (symbol "/", Div)]

aFactor :: Parser AExpr
aFactor = fmap AConst integer
          <|> fmap AVar identifier
          <|> brackets (symbol "(") aExpression (symbol ")")

main = do putStrLn "Please type an arithmetic expression involving +, -, *, /, integers or variables"
          forever (do putStr ">>> "
                      inp <- getLine
                      let out = parse aExpression inp
                      if null out then print "No parse" else print . fst . head $ out)
