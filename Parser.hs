module Parser where

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
import Evaluator
import qualified Text.Read as Read
import Prelude hiding (Left, Right)

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
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do x <- p
                    xs <- first (many (sep >> p))
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
                       dir <- (leftSymbol  >> return Left)
                          <|> (rightSymbol >> return Right)
                          <|> (upSymbol    >> return Up)
                          <|> (downSymbol  >> return Down)
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


-- A parser for skip statements. That is, it consumes the skip symbol and then consumes all remaining characters until
-- the end of the line.
skipStatement :: Parser Stmt
skipStatement = do skipSymbol
                   first . many $ sat (/='\n')
                   return Skip


preprocess :: String -> String
preprocess = unlines . (flip evalState [0]) . addIndents . (++ [""]) . filter (not . all isSpace) . lines

-- Adds an indent symbol each time the indentation level increases and a dedent symbol each time it decreases in the
-- given list of lines. This function utilizes the state monad to carry the stack of current indentation levels around.
addIndents :: [String] -> State [Int] [String]
addIndents []     = return []
addIndents (l:ls) = do indents <- get
                       let cur = length . takeWhile isSpace $ l
                       l' <- case compare cur (head indents) of
                               GT -> do modify (cur:)                                   -- Indent level increased,
                                        return ("{\n" ++ l)                             -- so output an indent symbol

                               LT -> do let indents' = dropWhile (>cur) indents         -- Indent level decreased, so
                                        put indents'                                    -- pop all bigger indents from
                                        let diff = length indents - length indents'     -- the stack and output just as
                                        return ((concat . replicate diff $ "}\n") ++ l) -- much dedent symbols

                               EQ -> return l                                           -- Indent level didn't change
                       liftM2 (:) (return l') (addIndents ls)

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
zeroSymbol       = symbol "🌕"
oneSymbol        = symbol "🌗"
twoSymbol        = symbol "🌓"
threeSymbol      = symbol "🌑"
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
