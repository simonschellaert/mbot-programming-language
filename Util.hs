module Util where
  
import WorldParser

rotateAround :: Coord -- Origin
             -> Angle -- Angle in radians
             -> Coord -- Coordinate that should be rotated
             -> Coord -- Updated coordinate
rotateAround (xo, yo) angle (x, y) = ( xo + (x - xo) * cos angle - (y - yo) * sin angle
                                     , yo + (x - xo) * sin angle + (y - yo) * cos angle)

-- Calculates the distance between 2 coordinates
distance :: Coord -> Coord -> Float
distance (x0, y0) (x1, y1) = sqrt ((x1 - x0) ** 2 + (y1 - y0) ** 2)
