import           Control.Concurrent (threadDelay)
import           Evaluator
import           Initialize         (initialize)
import qualified SimulatorInterface as S

main = do s <- S.openSimulator
          let mDevice = simulatorDevice s
          initialize mDevice

simulatorDevice s = Device {
    sleep        = \x -> threadDelay (x * 1000),
    setRGB       = \l r g b -> threadDelay 10 >> S.sendCommand s (S.setRGB l r g b),
    setMotor     = \l r -> threadDelay 10 >> S.sendCommand s (S.setMotor (l `quot` 5) (r `quot` 5)),
    readDistance = do val <- S.readUltraSonic s
                      threadDelay 10
                      return (round val * 40),
    readLine     = do line <- S.readLineFollower s
                      threadDelay 10
                      return line
}
