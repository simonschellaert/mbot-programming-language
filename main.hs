module Main where

import Control.Monad
import Parser
import Evaluator
import qualified System.Posix.Unistd as Unistd

main = do putStrLn "Please type an arithmetic expression involving +, -, *, /, integers or variablesss"
          inp <- readFile "demo.txt"
          let inp' = preprocess inp
          putStrLn inp'
          let out = parse statementSeq inp'
          print out
          unless (null out) (do let prog = fst (head out)
                                runEval logDevice (eval prog)
                                return ())

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




