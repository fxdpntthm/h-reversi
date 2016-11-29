{-# LANGUAGE OverloadedStrings #-}

module Grid where

import           Control.Applicative
import           Data.List.Split
import           Data.Text           hiding (chunksOf)
import           Debug.Trace
import           Graphics.Blank

type Cord = (Int, Int)

grid w h = do
        let sz = min w h
        let sqSize = sz / 9
        clearRect (0,0,w,h)
        beginPath()
        save()
        translate (w / 2, h / 2)
        lineWidth 3
        -- strokeStyle "cyan"
        -- sequence_ $ computeSquare (-sz/2, -sz/2) sqSize <$> gridCord 9
        -- stroke()
        -- save()
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
pointToSq :: (Double, Double)  -> Double -> Double -> Maybe (Int,Int)
pointToSq (x,y) w h = validate $
  do x' <- Just $ round $ ((x - w / 2) / sz) * 10
     y' <- Just $ round $ ((y - h / 2) / sz) * 10
     return (x', y')
  where sz = min w h

-- returns a Nothing if click is out of range
validate :: Maybe (Int, Int) -> Maybe (Int, Int)
validate c@(Just (x , y)) = if (x > 3 || x < -4) || (y > 3 || y < -4)
  then Nothing else c
validate Nothing = Nothing
