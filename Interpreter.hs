module Interpreter where

import           Control.Concurrent (threadDelay)
import           Evaluator
import           Initialize
import qualified MBot

main = do d <- MBot.openMBot
          let mDevice = botDevice d
          initialize mDevice
          MBot.closeMBot d

botDevice d = Device {
    sleep        = threadDelay . (*1000),
    setRGB       = \l r g b -> MBot.sendCommand d $ MBot.setRGB l r g b,
    setMotor     = \l r -> MBot.sendCommand d $ MBot.setMotor l r,
    readDistance = fmap round (MBot.readUltraSonic d),
    readLine     = fmap lineToInt (MBot.readLineFollower d)
}

lineToInt :: MBot.Line -> Int
lineToInt MBot.BOTHW  = 0
lineToInt MBot.RIGHTB = 1
lineToInt MBot.LEFTB  = 2
lineToInt MBot.BOTHB  = 3
