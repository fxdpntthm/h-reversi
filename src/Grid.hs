{-# LANGUAGE OverloadedStrings #-}

module Grid where

import           Control.Applicative
import           Data.List.Split
import           Data.Map            (Map)
import           Data.Text           hiding (chunksOf)
import           Debug.Trace
import           Disc
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

-- | Orientation of the line
-- whether it is North, south east, west, south-east, etc
data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq, Enum)

grid w h = do
        let sz = min w h
        let sqSize = sz / 9
        clearRect (0,0,w,h)
        beginPath()
        save()
        translate (w / 2, h / 2)
        lineWidth 3
        beginPath()
        strokeStyle "black"
        sequence_ $ computeSquare (-sz/2, -sz/2) sqSize <$> gridCord 8
        fillStyle "green"
        fill()
        stroke()
        restore()

gridCord n = [(x,y) | x <- [0..n-1], y <- [0..n-1]]

computeSquare (x0, y0) sz (x, y) = sqr (x0 + x*sz, y0 + y * sz, sz)
sqr (x, y, s) = rect (x, y, s, s)

-- Returns the square co-ordiantes of the click
pointToSq :: (Double, Double)  -> Double -> Double -> Maybe Cord
pointToSq (x,y) w h = validate $
  do x' <- Just $ round $ ((x - w / 2) / sz) * 10
     y' <- Just $ round $ ((y - h / 2) / sz) * 10
     return (x', y')
  where sz = min w h

-- returns a Nothing if click is out of range
validate :: Maybe Cord -> Maybe Cord
validate c@(Just (x , y)) = if (x > maxX || x < minX) || (y > maxY || y < minY)
  then Nothing else c
validate Nothing = Nothing


adjacent :: Cord -> [Cord]
adjacent (x, y) = Prelude.filter (\(a,b) -> a >= minX && a <= maxX
                                   && b >= minY && b <= maxY && (a,b) /= (x,y))
  $ (,) <$> [ x-1..x+1 ] <*> [ y-1..y+1 ]

direction :: Cord -> Cord -> Direction
direction (nc_x, nc_y) (oc_x, oc_y)
  | (nc_x > oc_x) && (nc_y > oc_y) = NE
  | (nc_x == oc_x) && (nc_y > oc_y) = N
  | (nc_x < oc_x) && (nc_y > oc_y) = NW
  | (nc_x < oc_x) && (nc_y == oc_y) = W
  | (nc_x < oc_x) && (nc_y < oc_y) = SW
  | (nc_x == oc_x) && (nc_y < oc_y) = S
  | (nc_x > oc_x) && (nc_y < oc_y) = SE
  | (nc_x > oc_x) && (nc_y == oc_y) = E

