module Main where

import qualified MBot
import Control.Exception
import Control.Monad
import Parser
import Evaluator
import System.Environment
import System.Exit
import System.IO
import Control.Concurrent (threadDelay)
import qualified SimulatorInterface as S


main = do --d <- MBot.openMBot
          --let mDevice = botDevice d
          s <- S.openSimulator
          let mDevice = simulatorDevice s
          --let mDevice = logDevice
          initialize mDevice
          --MBot.closeMBot d

initialize :: Device -> IO ()
initialize mDevice = do args <- getArgs
                        when (length args /= 1) (die "Expects the script to execute as only argument")
                        input <- catch (readFile . head $ args) (readHandler . head $ args)
                        let inp' = preprocess input
                        putStrLn inp'
                        let out = parse statementSeq inp'
                        print out
                        unless (null out) (do let prog = fst (head out)
                                              runStmt mDevice prog
                                              return ())

readHandler :: String -> IOError -> IO a
readHandler name _ = die ("Cannot open file: " ++ name)

prompt s = do
    putStr s
    line <- getLine
    return (read line)

logDevice = Device {
    sleep        = \x -> putStrLn ("Sleeping for " ++ show x ++ " milliseconds") >> threadDelay (x * 1000),
    setRGB       = \l r g b -> putStrLn ("Setting light " ++ show l ++ " to " ++ show r ++ ", " ++ show g ++ ", " ++ show b),
    setMotor     = \l r -> putStrLn ("Setting motor speed to " ++ show l ++ ", " ++ show r),
    playTone     = \f d -> putStrLn ("Playing tone with frequency " ++ show f ++ " for " ++ show d ++ " millis"),
    readDistance = prompt "Enter the measurement of the distance sensor ",
    readLine     = prompt "Enter the measurement of the line sensor (0, 1, 2 or 3)"
}

botDevice d = Device {
    sleep        = \x -> putStrLn ("Sleeping for " ++ show x ++ " milliseconds") >> threadDelay (x * 1000),
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
lineToInt MBot.BOTHW = 0
lineToInt MBot.RIGHTB = 1
lineToInt MBot.LEFTB = 2
lineToInt MBot.BOTHB = 3

simulatorDevice s = Device {
    sleep        = \x -> putStrLn ("Sleeping for " ++ show x ++ " milliseconds") >> threadDelay (x * 1000),
    setRGB       = \l r g b -> putStrLn ("Setting light " ++ show l ++ " to " ++ show r ++ ", " ++ show g ++ ", " ++ show b) >> S.sendCommand s (S.setRGB l r g b),
    setMotor     = \l r -> putStrLn ("Setting motor speed to " ++ show l ++ ", " ++ show r) >> S.sendCommand s (S.setMotor l r),
    playTone     = \f t -> putStrLn ("Playing tone with frequency " ++ show f ++ " for " ++ show t ++ " millis"),
    readDistance = (do val <- S.readUltraSonic s
                       putStrLn ("read distance " ++ show val)
                       return (round val)),
    readLine     = (do line <- S.readLineFollower s
                       putStrLn ("did read line " ++ show line)
                       return line)
}
