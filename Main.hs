module Main where

import Control.Exception
import Control.Monad
import Parser
import Evaluator
import System.Environment
import System.Exit
import System.IO
import qualified System.Posix.Unistd as Unistd

main = do args <- getArgs
          when (length args /= 1) (die "Expects the script to execute as only argument")
          input <- catch (readFile . head $ args) (readHandler . head $ args)
          let inp' = preprocess input
          putStrLn inp'
          let out = parse statementSeq inp'
          print out
          unless (null out) (do let prog = fst (head out)
                                runEval logDevice (eval prog)
                                return ())

readHandler :: String -> IOError -> IO a
readHandler name _ = die ("Cannot open file: " ++ name)

prompt s = do
    putStr s
    line <- getLine
    return (read line)

logDevice = Device {
    sleep        = \x -> putStrLn ("Sleeping for " ++ show x ++ " milliseconds") >> Unistd.usleep (x * 1000),
    setRGB       = \l r g b -> putStrLn ("Setting light " ++ show l ++ " to " ++ show r ++ ", " ++ show g ++ ", " ++ show b),
    setMotor     = \l r -> putStrLn ("Setting motor speed to " ++ show l ++ ", " ++ show r),
    playTone     = \f d -> putStrLn ("Playing tone with frequency " ++ show f ++ " for " ++ show d ++ " millis"),
    readDistance = prompt "Enter the measurement of the distance sensor ",
    readLine     = prompt "Enter the measurement of the line sensor (0, 1, 2 or 3)"
}




