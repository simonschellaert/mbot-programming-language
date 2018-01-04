
module Simulator where

import           Control.Concurrent (threadDelay)
import           Evaluator
--import           Main               (initialize)
import qualified SimulatorInterface as S

main = do s <- S.openSimulator
          let device = simulatorDevice s
          setRGB device 1 0 0 100
          setRGB device 2 100 0 0
          sleep device 4000
          setRGB device 1 100 0 0
          setRGB device 2 0 0 100
          sleep device 4000
          let device = simulatorDevice s
          setRGB device 1 0 0 100
          setRGB device 2 100 0 0
          sleep device 4000
          setRGB device 1 100 0 0
          setRGB device 2 0 0 100
          sleep device 4000
          let device = simulatorDevice s
          setRGB device 1 0 0 100
          setRGB device 2 100 0 0
          sleep device 4000
          setRGB device 1 100 0 0
          setRGB device 2 0 0 100
          sleep device 4000
          let device = simulatorDevice s
          setRGB device 1 0 0 100
          setRGB device 2 100 0 0
          sleep device 4000
          setRGB device 1 100 0 0
          setRGB device 2 0 0 100
          sleep device 4000
          let device = simulatorDevice s
          setRGB device 1 0 0 100
          setRGB device 2 100 0 0
          sleep device 4000
          setRGB device 1 100 0 0
          setRGB device 2 0 0 100
          sleep device 4000
          let device = simulatorDevice s
          setRGB device 1 0 0 100
          setRGB device 2 100 0 0
          sleep device 4000
          setRGB device 1 100 0 0
          setRGB device 2 0 0 100
          sleep device 400000
          threadDelay 10000000000

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
