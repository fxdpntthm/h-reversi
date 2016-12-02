module Disc where

import           Data.Map
import qualified Data.Map       as Map
import           Data.Text
import           Graphics.Blank
import           Util
data Disc = White | Black deriving (Show, Eq, Ord)

-- | Swaps the turn
swap :: Disc -> Disc
swap White = Black
swap Black = White

isBlack = (== Black)
isWhite = not . isBlack

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

drawDiscs sz board =
     sequence_ [ do save()
                    translate (sz / 2
                                + (1.8 * sz / 9)
                                + fromIntegral x * (sz / 9)
                              , sz / 2
                                -- + (0.5 * sz/9)
                                + fromIntegral y * (sz / 9))
                    case Map.lookup (x,y) board of
                      Just d  -> drawDisc (sz / 32) d
                      Nothing -> return ()
                    restore()
               | x <- [minX..maxX::Int]
               , y <- [minY..maxY::Int]
               ]
