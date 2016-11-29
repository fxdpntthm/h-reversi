module Disc where

import           Data.Map
import           Data.Text
import           Graphics.Blank

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
