import           Control.Concurrent               (MVar, forkIO, modifyMVar_,
                                                   newMVar, putMVar, readMVar,
                                                   swapMVar, takeMVar,
                                                   threadDelay, tryTakeMVar,
                                                   withMVar)
import           Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G
import           System.Exit
import           WorldParser

-- The size of one cell
cell = 32.0

render :: G.Picture -> World -> G.Picture
render wp (World robot walls _) = G.pictures $
                                  [renderPicAt (G.rotate angle $ robotPicture robot) position]
                                  ++ map (renderPicAt wp) walls
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

step :: MVar World -> Float -> World -> IO World
step m t _ = do world <- takeMVar m
                putMVar m $ tick t world
                return world

tick :: Float -> World -> World
tick t (World robot walls wLines) = World robot {rPosition = position', rAngle = angle' } walls wLines
  where (position', angle') = nextPosition t robot


nextPosition :: Float -> Robot -> (Coord, Angle)
nextPosition t robot = ((x', y'), angle')
  where (x, y) = rPosition robot
        angle = rAngle robot
        vl = fromIntegral $ rSpeedLeft robot
        vr = fromIntegral $ rSpeedRight robot
        wr = 0.03
        wa = 0.15
        deltaX = t * (wr / 2) * (vl + vr) * cos angle
        deltaY = t * (wr / 2) * (vl + vr) * sin angle
        deltaAngle = t * (wr / wa) * (vr - vl)
        x' = x + deltaX
        y' = y + deltaY
        angle' = angle + deltaAngle -- TODO modulo 'n stuff

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

openSimulator :: World -> IO Simulator
openSimulator world = do
  m <- newMVar world
  let s = Simulator m
  _ <- forkIO (runSimulator m)
  return s

sendCommand :: Simulator -> (World -> World) -> IO ()
sendCommand (Simulator m ) command = do world <- takeMVar m
                                        let world' = command world
                                        putMVar m world'

setRGB :: Int -> Int -> Int -> Int -> World -> World
setRGB side  r g b world = case side of
  1 -> world { wRobot = robot { rColorLeft=(r, g, b)}}
  2 -> world { wRobot = robot { rColorRight=(r, g, b)}}
  where robot = wRobot world

setMotor :: Int -> Int -> World -> World
setMotor l r world = world { wRobot = robot {rSpeedLeft = l, rSpeedRight = r}}
  where robot = wRobot world

main = do txt <- readFile "worlds/world1.txt"
          let world = makeWorld txt
          s <- openSimulator world
          sendCommand s $ setMotor (-255) 255
          sendCommand s $ setRGB 1 255 255 255
          threadDelay 1000000
          sendCommand s $ setRGB 2 255 255 255
          threadDelay 1000000
          sendCommand s $ setRGB 1 0 0 0
          threadDelay 10000000000
