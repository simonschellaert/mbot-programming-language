import           Control.Concurrent               (MVar, forkIO, newMVar,
                                                   putMVar, readMVar, takeMVar,
                                                   threadDelay)
import           Data.Fixed
import           Data.Maybe                       (mapMaybe)
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Angle
import           Graphics.Gloss.Geometry.Line
import qualified Graphics.Gloss.Interface.IO.Game as G
import           WorldParser

-- The size of one cell.
cell = 32.0

-- Renders the world.
render :: G.Picture -- Picture of a wall
       -> World     -- The world that should be rendered
       -> G.Picture -- Picture of the world
render wp (World robot walls lns) = G.pictures $
                                  [renderPicAt (G.rotate angle $ robotPicture robot) position]
                                  ++ map (\ln -> renderPicAt (linePicture ln) $ fst ln)  lns
                                  ++ map (renderPicAt wp) walls
  where position = rPosition robot
        angle = radToDeg $ rAngle robot
        size which = maximum $ map which walls
        toPix which = (+ (cell / 2 - cell * size which / 2))
                      . (* cell)
        renderPicAt picture (x, y) = G.translate (toPix fst x)
                                                 (toPix snd y * (-1.0))
                                                 picture

-- Creates a picture of a line.
linePicture :: Line -> G.Picture
linePicture ((x0, y0), (x1, y1)) = G.rotate angle
                                   $ G.translate ((l - cell) / 2) 0
                                   $ G.rectangleSolid l cell
  where dx = x1 - x0
        dy = y1 - y0
        d = sqrt (dx ** 2 + dy ** 2)
        l = d * cell
        angle = radToDeg $ argV (dx, dy)

-- Creates a picture of the robot.
robotPicture :: Robot -> G.Picture
robotPicture robot = G.pictures
  [ G.color G.blue $ G.rectangleSolid robotSize robotSize               -- base
  , G.color cLeft $ G.Translate 3 6 $ G.circleSolid 3                   -- right led
  , G.color cRight $ G.Translate 3 (-6) $ G.circleSolid 3               -- left led
  , G.translate 0 (robotSize / 2) $ G.rectangleSolid (robotSize / 2) 4  -- right wheel
  , G.translate 0 (-robotSize / 2) $ G.rectangleSolid (robotSize / 2) 4 -- left wheel
  ]
  where makeColor (r,g,b) = G.makeColorI r g b 255
        cLeft = makeColor $ rColorLeft robot
        cRight = makeColor $ rColorRight robot
        robotSize = cell * 1.0

step :: MVar World -> Float -> World -> IO World
step m t _ = do world <- takeMVar m
                putMVar m $ updateWorld t world
                return world

-- Updates the world by moving the robot in the world based on how much time has passed.
-- Collision detection is taken into account when moving the robot.
updateWorld :: Float -- Period of time (in seconds) needing to be advanced
            -> World -- Old version of the world
            -> World -- New version of the world
updateWorld t world@(World robot walls wLines)
 | collides walls robot' = world
 | otherwise             = world {wRobot = robot' }
 where robot' = nextPosition t robot

-- Calculates the next position of the robot.
-- Collision detection is NOT taken into account.
nextPosition :: Float -- Period of time (in seconds) needing to be advanced
             -> Robot -- The robot
             -> Robot -- The updated robot
nextPosition t robot = robot {rPosition = (x', y'), rAngle = angle'}
  where (x, y) = rPosition robot
        angle = rAngle robot
        vl = fromIntegral $ rSpeedLeft robot
        vr = fromIntegral $ rSpeedRight robot
        wr = 0.03
        wa = 0.11
        x' = x + t * (wr / 2) * (vl + vr) * cos angle
        y' = y + t * (wr / 2) * (vl + vr) * sin angle
        angle' = (angle + t * (wr / wa) * (vr - vl)) `mod'` (2 * pi)

-- Returns true when the position of the robot collides with one or more walls.
collides :: [Coord] -- A list with the coordinates of the walls
         -> Robot   -- The robot with the updated position
         -> Bool    -- True when this position collides with one ore more walls
collides walls (Robot _ _ (x, y) angle _ _) = not $ null [corner | corner <- corners, wall <- walls, isInSquare wall corner]
  where corners = map (rotateAround (x + 0.5, y + 0.5) angle)
                      [ (x, y)
                      , (x + 0.5, y)
                      , (x + 1, y)
                      , (x, y + 0.5)
                      , (x, y + 1)
                      , (x + 0.5, y + 1)
                      , (x + 1.0, y + 0.5)
                      , (x + 1.0, y + 1.0)]
        isInSquare (xSquare, ySquare) (x', y') =  x' > xSquare
                                               && x' < xSquare + 1.0
                                               && y' > ySquare
                                               && y' < ySquare + 1.0



runSimulator :: MVar World -> IO ()
runSimulator m = do world <- readMVar m
                    [wp] <- mapM loadBMP ["images/wall.bmp"]
                    G.playIO (G.InWindow "MBot" (700,500) (0,0)) -- display
                               G.white                           -- background
                               60                                -- fps
                               world                             -- initial world
                               (return . render wp)              -- render world
                               (const return)                    -- handle input
                               (step m)                          -- step world in time



newtype Simulator = Simulator (MVar World)
type Command = World -> World

-- Creates a new simulator and runs it on another thread.
openSimulator :: IO Simulator
openSimulator = do
  txt <- readFile "worlds/world1.txt"
  let world = makeWorld txt
  m <- newMVar world
  let s = Simulator m
  _ <- forkIO (runSimulator m)
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
-- 0 = both sensors detect a black line
-- 1 = the right sensor detects a black line
-- 2 = the left sensor detects a black line
-- 3 = no sensor detects a black line
readLineFollower :: Simulator -> IO Int
readLineFollower (Simulator m) = do world <- readMVar m
                                    return $ getLineStatus world

-- Calculates the line status
getLineStatus :: World ->  Int
getLineStatus world@(World robot _ lns)
  | isOnBlack left && isOnBlack right = 0
  | isOnBlack right = 1
  | isOnBlack left = 2
  | otherwise = 3
  where (x, y) = rPosition robot
        angle = rAngle robot
        left = rotateAround (x + 0.5, y + 0.5) angle (x + 1.0, y)
        right =  rotateAround (x + 0.5, y + 0.5) angle (x + 1.0, y + 1.0)
        closestPoint = uncurry closestPointOnLine
        isOnLine point ln = distance (closestPoint ln point) point <= 1 / 2
        isWithinBounds (xa, ya) ((x0, y0), (x1, y1)) =  (x0 <= xa && xa <= x1)
                                                     || (x1 <= xa && xa <= x0)
                                                     || (y0 <= ya && ya <= y1)
                                                     || (y1 <= ya && ya <= y0)
        lns' = map adjustLine lns
        isOnBlack point = any (\ln -> isWithinBounds point ln
                                      && isOnLine point ln) lns'

-- Translates the coordinates so that they will be in the middle of a square instead of the beginning.
adjustLine :: Line -> Line
adjustLine ((x0, y0), (x1, y1)) = ( (x0 + sin angle / 2, y0 + cos angle / 2)
                                  , (x1 + sin angle / 2, y1 + cos angle / 2))
  where angle = radToDeg $ argV (x1 - x0, y1 - y0)

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


rotateAround :: Coord -- Origin
             -> Angle -- Angle in radians
             -> Coord -- Coordinate that should be rotated
             -> Coord -- Updated coordinate
rotateAround (xo, yo) angle (x, y) = ( xo + (x - xo) * cos angle - (y - yo) * sin angle
                                     , yo + (x - xo) * sin angle + (y - yo) * cos angle)

-- Calculates the distance between 2 coordinates
distance :: Coord -> Coord -> Float
distance (x0, y0) (x1, y1) = sqrt ((x1 - x0) ** 2 + (y1 - y0) ** 2)
