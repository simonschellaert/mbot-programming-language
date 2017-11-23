import Data.List
import Data.Maybe

type X = Integer
type Y = Integer
type Coord = (X, Y)
type Angle = Double

type Line = (Coord, Coord)

data World = World { wRobot   :: (Coord, Angle)
                   , wWalls   :: [Coord]
                   , wLines   :: [Line]
                   } deriving (Eq, Ord, Show)

emptyWorld = World { wRobot = ((0, 0), 0), wWalls = [], wLines = [] }

addPiece :: Coord -> Char -> World -> World
addPiece co ch w
    | ch `elem` wallChars = w { wWalls = co:wWalls w }
    | ch `elem` botChars  = w { wRobot = (co, angle) }
    | otherwise           = w
    where wallChars = ['X', '+', '|', '-']
          botChars  = ['>', '^', '<', 'v']
          angle     = (fromIntegral . fromJust $ elemIndex ch botChars) * pi/2


-- Adds the pieces specified in the ASCII input representation of the grid world to the world.
-- Note that this method expects only the representation of the grid itself and not the line segments underneath.
addPieces :: World -> String -> World
addPieces w txt = foldr (uncurry addPiece) w withCoords
    where withCoords = [((x, y), c) | (y, line) <- zip [0..] (lines txt), (x, c) <- zip [0..] line]

-- Adds line segments to the world based on a sequence of line segments represented in the format "(x1, y1), (x2, y2)".
addLines :: World -> [String] -> World
addLines w [] = w
addLines w (l:ls) = w { wLines = (x, y):wLines (addLines w ls) }
    where [x, y] = map read (words l)

-- Creates a world based on the ASCII input representation of that world.
makeWorld :: String -> World
makeWorld txt = addLines (addPieces emptyWorld (unlines pcs)) lns
    where (pcs, lns) = span (notElem '(') (lines txt)

main = readFile "world1.txt" >>= (print . makeWorld)
