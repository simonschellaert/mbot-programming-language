module Main where

import qualified MBot
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
          --d <- MBot.openMBot
          let mDevice = logDevice -- or "botDevice d"
          unless (null out) (do let prog = fst (head out)
                                runEval mDevice (eval prog)
                                return ())
          --MBot.closeMBot d

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

botDevice d = Device {
    sleep        = \x -> putStrLn ("Sleeping for " ++ show x ++ " milliseconds") >> Unistd.usleep (x * 1000),
    setRGB       = \l r g b -> putStrLn ("Setting light " ++ show l ++ " to " ++ show r ++ ", " ++ show g ++ ", " ++ show b) >> (MBot.sendCommand d $ MBot.setRGB l r g b),
    setMotor     = \l r -> putStrLn ("Setting motor speed to " ++ show l ++ ", " ++ show r) >> (MBot.sendCommand d $ MBot.setMotor l r),
    playTone     = \f t -> putStrLn ("Playing tone with frequency " ++ show f ++ " for " ++ show t ++ " millis") >> (MBot.sendCommand d $ MBot.playTone f t),
    readDistance = (do val <- MBot.readUltraSonic d
                       putStrLn ("read distance " ++ show val)
                       return (round val)),
    readLine     = (do val <- MBot.readLineFollower d
                       let line = lineToInt val
                       putStrLn ("did read line " ++ show line)
                       return line)
}

lineToInt :: MBot.Line -> Int
lineToInt MBot.BOTHB = 0
lineToInt MBot.LEFTB = 1
lineToInt MBot.RIGHTB = 2
lineToInt MBot.BOTHW = 3



