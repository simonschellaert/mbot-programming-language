module Gui where

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
import           Util
import           WorldParser

-- The size of one cell.
cell = 32.0

-- The thickness of a line.
lineWidth = 8.0

-- Renders the world.
render :: G.Picture -- Picture of a wall
       -> World     -- The world that should be rendered
       -> G.Picture -- Picture of the world
render wp (World robot walls lns) = G.pictures $
                                     map (\ln -> renderPicAt (linePicture ln) $ fst ln)  lns
                                  ++ map (renderPicAt wp) walls
                                  ++ [renderPicAt (G.rotate angle $ robotPicture robot) position]

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
                                   $ G.rectangleSolid l lineWidth
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
        angle' = (angle + t * (wr / wa) * (vl - vr)) `mod'` (2 * pi)

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
                    G.playIO (G.InWindow "MBot" (300,200) (0,0)) -- display
                               G.white                           -- background
                               60                                -- fps
                               world                             -- initial world
                               (return . render wp)              -- render world
                               (const return)                    -- handle input
                               (step m)                          -- step world in time
