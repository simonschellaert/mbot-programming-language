module Simulator where

import           Control.Concurrent (threadDelay)
import           Evaluator
--import           Main               (initialize)
import qualified SimulatorInterface as S

main = do s <- S.openSimulator
          S.sendCommand s $ S.setMotor 5 5
          loop s

loop s = do val <- S.readLineFollower s
            print val
            loop s
