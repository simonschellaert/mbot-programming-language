module Initialize where

import           Control.Exception
import           Control.Monad
import           Evaluator
import           Parser
import           System.Environment
import           System.Exit
import           System.IO

-- Parses the arguments passed to the program and executes the script passed as the argument on the specified device.
initialize :: Device -> IO ()
initialize mDevice = do args <- getArgs
                        when (length args /= 1) (die "Expects the script to execute as only argument")
                        input <- catch (readFile . head $ args) (readHandler . head $ args)
                        let prs = parse statementSeq . preprocess $ input
                        unless (null prs) (do let prog = fst (head prs)
                                              void $ runStmt mDevice prog)

readHandler :: String -> IOError -> IO a
readHandler name _ = die ("Cannot open file: " ++ name)
