module Interpreter where

import           Control.Concurrent (threadDelay)
import           Evaluator
import           Initialize
import qualified MBot

main = do --d <- MBot.openMBot
            --let mDevice = botDevice d
          let mDevice = logDevice
          initialize mDevice
            --MBot.closeMBot d

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
lineToInt MBot.BOTHW  = 0
lineToInt MBot.RIGHTB = 1
lineToInt MBot.LEFTB  = 2
lineToInt MBot.BOTHB  = 3
