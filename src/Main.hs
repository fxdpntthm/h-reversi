{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Data.Map
import qualified Data.Map                    as Map
import           Debug.Trace
import           Disc
import           Graphics.Blank
import           Grid
main :: IO ()
main = do
  boardV <- newTVarIO Map.empty
  -- generate some static data for rendering
  sampleData boardV
  blankCanvas 3000 {events = ["mousedown"] } $ \context -> viewer context boardV

sampleData :: TVar (Map Cord Disc) -> IO ()
sampleData boardV = atomically $ do
  board <- readTVar boardV
  writeTVar boardV (fromList [((-4,-4), White),
                              ((-1,-1), Black),
                              ((-1,0), White),
                              ((0,0), Black),
                              ((0,-1), White),
                              ((3,3), Black)])

viewer :: DeviceContext -> TVar (Map Cord Disc) -> IO ()
viewer context boardV = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  board <- atomically $ readTVar boardV
  print board
  send context $ do
    clearRect (0,0, cw, ch)
    beginPath()
    grid (width context) (height context)
    sequence_ [ do save()
                   translate (sz / 2 - (fromIntegral x - 0.5) * (sz / 9)
                             , sz / 2 - fromIntegral (y+1) * (sz / 9))
                   case Map.lookup (x,y) board of
                       Just d  -> drawDisc (sz / 32) d
                       Nothing -> return ()
                   restore()
              | x <- [-4..3::Int]
              , y <- [-4..3::Int]
              ]
    -- print $ (width context, height context)
    return ()
  event <- wait context
  --print $ ePageXY event
  print $ ePageXY event >>= \ (x, y) -> pointToSq (x, y) cw ch
  viewer context boardV

