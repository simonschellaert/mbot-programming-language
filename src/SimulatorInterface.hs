module SimulatorInterface where

import           Control.Concurrent            (MVar, forkIO, newMVar, putMVar,
                                                readMVar, takeMVar, threadDelay)
import           Data.Fixed
import           Data.Maybe                    (mapMaybe)
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Angle
import           Graphics.Gloss.Geometry.Line
import           Gui
import           Util
import           WorldParser

newtype Simulator = Simulator (MVar World)
type Command = World -> World

-- Creates a new simulator and runs it on another thread.
openSimulator :: IO Simulator
openSimulator = do
    world <- fmap makeWorld (readFile "worlds/world1.txt")
    m <- newMVar world
    let s = Simulator m
    forkIO (runSimulator m)
    return s

-- Sends a command to the simulator.
sendCommand :: Simulator -> Command -> IO ()
sendCommand (Simulator m ) command = do world <- takeMVar m
                                        let world' = command world
                                        putMVar m world'

-- Creates a command that changes the rgb values of the leds of the robot
setRGB :: Int -- The left that should be changed. 1 = left and 2 = right
       -> Int -- red
       -> Int -- green
       -> Int -- blue
       -> Command
setRGB side r g b world = case side of
  1 -> world { wRobot = robot { rColorLeft=(r, g, b)}}
  2 -> world { wRobot = robot { rColorRight=(r, g, b)}}
  where robot = wRobot world

-- Creates a command that sets the speed of the motors of the robot.
setMotor :: Int     -- Speed of the left wheel
         -> Int     -- Speed of the right wheel
         -> Command
setMotor l r world = world { wRobot = robot {rSpeedLeft = l, rSpeedRight = r}}
  where robot = wRobot world

-- Returns the distance between the front of the robot and the nearest wall.
readUltraSonic :: Simulator -> IO Float
readUltraSonic (Simulator m) = do world <- readMVar m
                                  return $ getDistance world

-- Calculates the distance between the front of the robot and the nearest wall.
-- The front of the robot is equal to (x + 1, y + 0.5) when the robot is looking to the right
-- where (x, y) is the coordinate of the robot.
getDistance :: World -> Float
getDistance world@(World robot walls _) = minimum $ map (distance (xo, yo)) intersections
  where position@(x, y) = rPosition robot
        angle = rAngle robot
        origin@(xo, yo) = rotateAround (x + 0.5, y + 0.5) angle (x + 1.0, y + 0.5)
        end = rotateAround (x + 0.5, y + 0.5) angle (x + 50, y + 0.5)
        intersectionsWith (x', y') = mapMaybe (uncurry $ intersectSegSeg origin end)
                                     [ ((x', y'), (x' + 1, y'))
                                     , ((x' + 1, y'), (x' + 1, y' + 1))
                                     , ((x' + 1, y' + 1), (x', y' + 1))
                                     , ((x', y' + 1), (x', y'))
                                     ]
        intersections = concatMap intersectionsWith walls

-- Reads the line status of the robot.
-- The possible return values are equal to:
-- 0 = no sensor detects a black line
-- 1 = the right sensor detects a black line
-- 2 = the left sensor detects a black line
-- 3 = both sensors detect a black line
readLineFollower :: Simulator -> IO Int
readLineFollower (Simulator m) = do world <- readMVar m
                                    return $ getLineStatus world

-- Calculates the line status
getLineStatus :: World ->  Int
getLineStatus world@(World robot _ lns)
  | isOnBlack left && isOnBlack right = 3
  | isOnBlack right = 1
  | isOnBlack left = 2
  | otherwise = 0
  where (x, y) = rPosition robot
        angle = rAngle robot
        left = rotateAround (x + 0.5, y + 0.5) angle (x + 1.0, y + 0.4)
        right =  rotateAround (x + 0.5, y + 0.5) angle (x + 1.0, y + 0.6)
        isOnSegment point (start, end) = distance closest point <= 1 / 2
                                       && abs (distance start end - (distance start closest + distance closest end)) < 0.001
          where closest = closestPointOnLine start end point
        lns' = map adjustLine lns
        isOnBlack point = any (isOnSegment point) lns'

-- Translates the coordinates so that they will be in the middle of a square instead of the beginning.
adjustLine :: Line -> Line
adjustLine ((x0, y0), (x1, y1)) = ( (x0 + sin angle / 2, y0 + cos angle / 2)
                                  , (x1 + sin angle / 2, y1 + cos angle / 2))
  where angle = argV (abs (x1 - x0), abs (y1 - y0))
