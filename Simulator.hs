import           Graphics.Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as G
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


handleEvent :: G.Event -> World -> World
handleEvent _ world = world

step :: Float -> World -> World
step _ (World robot walls wLines) = World robot {rPosition = (x+0.1,y) } walls wLines
  where (x, y) = rPosition robot

main = do txt <- readFile "worlds/world1.txt"
          let world = makeWorld txt
          [wp]      <- mapM loadBMP ["images/wall.bmp"]
          G.play (G.InWindow "MBot" (700,500) (0,0))            -- display
                            G.white                             -- background
                            10                                  -- fps
                            world                               -- initial world
                            (render wp)                         -- render world
                            handleEvent                         -- handle input
                            step                                -- step world in time
