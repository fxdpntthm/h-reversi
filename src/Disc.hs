{-# LANGUAGE OverloadedStrings #-}
module Disc where

import           Data.Text
import           Graphics.Blank

data Disc = White | Black deriving (Show, Eq, Ord)

swap :: Disc -> Disc
swap White = Black
swap Black = White

drawDisc :: Double -> Disc -> Canvas ()
drawDisc radius disc = do
  beginPath()
  arc(0, 0, radius, 0, 2 * pi, False)
  fillStyle $ pack $ clr disc
  fill()
  lineWidth 5
  strokeStyle $ pack $ clr disc
  stroke()

clr :: Disc -> String
clr Black = "#000000"
clr White = "#ffffff"
