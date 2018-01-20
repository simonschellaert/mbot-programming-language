module Initialize where

import           Control.Exception
import           Control.Monad
import           Evaluator
import           Parser
import           System.Environment
import           System.Exit
import           System.IO

initialize :: Device -> IO ()
initialize mDevice = do args <- getArgs
                        when (length args /= 1) (die "Expects the script to execute as only argument")
                        input <- catch (readFile . head $ args) (readHandler . head $ args)
                        let inp' = preprocess input
                        putStrLn inp'
                        let out = parse statementSeq inp'
                        unless (null out) (do let prog = fst (head out)
                                              runStmt mDevice prog
                                              return ())

readHandler :: String -> IOError -> IO a
readHandler name _ = die ("Cannot open file: " ++ name)
