import           Control.Concurrent               (MVar, forkIO, newEmptyMVar,
                                                   putMVar, takeMVar,
                                                   threadDelay, tryTakeMVar)
import           Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import           WorldParser

data Side = SideLeft | SideRight

data RobotCommand = SetMotor Side Int
                  | SetRGB  Side Int Int Int

-- The size of one cell
cell = 32.0

render :: G.Picture -> World -> IO G.Picture
render wp (World robot walls _) = return (
                                         G.pictures $
                                         [renderPicAt (G.rotate angle $ robotPicture robot) position]
                                         ++ map (renderPicAt wp) walls
                                         )
  where position = rPosition robot
        angle = rAngle robot
        size which = maximum $ map which walls
        toPix which = (+ (cell / 2 - cell * size which / 2))
                      . (* cell)
        renderPicAt picture (x, y) = G.translate (toPix fst x)
                                                 (toPix snd y * (-1.0))
                                                 picture


robotPicture :: Robot -> G.Picture
robotPicture robot = G.pictures
  [ G.color G.blue $ G.rectangleSolid cell cell               -- base
  , G.color cLeft $ G.Translate 3 6 $ G.circleSolid 3         -- right led
  , G.color cRight $ G.Translate 3 (-6) $ G.circleSolid 3     -- left led
  , G.translate 0 (cell / 2) $ G.rectangleSolid (cell / 2) 4  -- right wheel
  , G.translate 0 (-cell / 2) $ G.rectangleSolid (cell / 2) 4 -- left wheel
  ]
  where makeColor (r,g,b) = G.makeColorI r g b 255
        cLeft = makeColor $ rColorLeft robot
        cRight = makeColor $ rColorRight robot


handleEvent :: G.Event -> World -> IO World
handleEvent _ = return

step :: MVar RobotCommand -> Float -> World -> IO World
step m _ world = do maybeCommand <- tryTakeMVar m
                    case maybeCommand of
                      Just command -> executeCommand command world
                      Nothing      -> return world

executeCommand :: RobotCommand -> World -> IO World
executeCommand command world = return $ case command of
                                         SetRGB SideLeft r g b  -> world { wRobot = robot { rColorLeft=(r, g, b)}}
                                         SetRGB SideRight r g b -> world { wRobot = robot { rColorRight=(r, g, b)}}
                                         _                      -> world

  where robot = wRobot world

launchSimulator :: MVar RobotCommand -> IO ()
launchSimulator m = do txt <- readFile "worlds/world1.txt"
                       let world = makeWorld txt
                       [wp] <- mapM loadBMP ["images/wall.bmp"]
                       G.playIO (G.InWindow "MBot" (700,500) (0,0)) -- display
                               G.white                             -- background
                               10                                   -- fps
                               world                               -- initial world
                               (render wp)                         -- render world
                               handleEvent                         -- handle input
                               (step m)                            -- step world in time

main = do m <- newEmptyMVar
          _ <- forkIO (launchSimulator m)
          putMVar m (SetRGB SideLeft 255 255 255)
          threadDelay 1000000
          putMVar m (SetRGB SideRight 255 255 255)
          threadDelay 1000000
          putMVar m (SetRGB SideLeft 0 0 255)
          threadDelay 1000000
          putMVar m (SetRGB SideRight 0 0 255)
          threadDelay 1000000
          putMVar m (SetRGB SideLeft 0 255 0)
          threadDelay 1000000
          putMVar m (SetRGB SideRight 0 255 0)
          threadDelay 1000000
          putMVar m (SetRGB SideLeft 255 0 0)
          threadDelay 1000000
          putMVar m (SetRGB SideRight 255 0 0)
          threadDelay 1000000
