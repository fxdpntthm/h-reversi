{-# LANGUAGE OverloadedStrings #-}
module Disc where

import           Data.Text
import           Graphics.Blank

data Disc = White | Black deriving (Show, Eq, Ord)

flip :: Disc -> Disc
flip White = Black
flip Black = White

drawDisc :: Double -> Disc -> Canvas ()
drawDisc radius disc = do
  beginPath()
  arc(0, 0, radius, 0, 2 * pi, False)
  fillStyle $ pack $ clr disc
  fill()
  lineWidth 5
  strokeStyle "cyan"
  stroke()

clr :: Disc -> String
clr Black = "#000000"
clr White = "#ffffff"
