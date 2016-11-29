module Disc where

import           Data.Map
import           Data.Text
import           Graphics.Blank

-- | Coordinate system goes from -4 to 3
type Cord = (Int, Int)

minX :: Int
minX = -4

maxX :: Int
maxX = 3

minY :: Int
minY = -4

maxY :: Int
maxY = 3

type Board = Map Cord Disc

data Disc = White | Black deriving (Show, Eq, Ord)

-- | Swaps the turn
swap :: Disc -> Disc
swap White = Black
swap Black = White

-- | Draws the disc in the appropriate position
drawDisc :: Double -> Disc -> Canvas ()
drawDisc radius disc = do
  beginPath()
  arc(0, 0, radius, 0, 2 * pi, False)
  fillStyle $ pack $ clr disc
  fill()
  lineWidth 5
  strokeStyle $ pack $ clr disc
  stroke()

-- | Returns the color of the disk
clr :: Disc -> String
clr Black = "#000000"
clr White = "#ffffff"

adjacent :: Cord -> [Cord]
adjacent (x, y) = Prelude.filter (\(a,b) -> a >= minX && a <= maxX
                                   && b >= minY && b <= maxY && (a,b) /= (x,y))
  $ (,) <$> [ x-1..x+1 ] <*> [ y-1..y+1 ]
