module Simulator where

import           Control.Concurrent (threadDelay)
import           Evaluator
import           Initialize         (initialize)
import qualified SimulatorInterface as S

main = do s <- S.openSimulator
          let mDevice = simulatorDevice s
          initialize mDevice

simulatorDevice s = Device {
    sleep        = \x -> putStrLn ("Sleeping for " ++ show x ++ " milliseconds") >> threadDelay (x * 1000),
    setRGB       = \l r g b -> putStrLn ("Setting light " ++ show l ++ " to " ++ show r ++ ", " ++ show g ++ ", " ++ show b) >> S.sendCommand s (S.setRGB l r g b),
    setMotor     = \l r -> putStrLn ("Setting motor speed to " ++ show l ++ ", " ++ show r) >> S.sendCommand s (S.setMotor (l `quot` 5) (r `quot` 5)),
    playTone     = \f t -> putStrLn ("Playing tone with frequency " ++ show f ++ " for " ++ show t ++ " millis"),
    readDistance = (do val <- S.readUltraSonic s
                       putStrLn ("read distance " ++ show val)
                       return (round val * 40)),
    readLine     = (do line <- S.readLineFollower s
                       putStrLn ("did read line " ++ show line)
                       return line)
}
