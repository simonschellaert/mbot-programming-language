
import           Control.Concurrent (threadDelay)
import           SimulatorInterface

main = do s <- openSimulator
          sendCommand s $ setMotor 10 11
          sendCommand s $ setRGB 1 255 255 255
          threadDelay 10000000
          sendCommand s $ setMotor (-10) (-10)
          sendCommand s $ setRGB 1 255 0 0
          sendCommand s $ setRGB 2 255 0 0
          threadDelay 10000000
          sendCommand s $ setMotor 9 10
          sendCommand s $ setRGB 1 0 255 0
          sendCommand s $ setRGB 2 1 255 0
          threadDelay 1000000000
