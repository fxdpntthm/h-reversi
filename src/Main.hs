{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
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
  blankCanvas 3000 {events = ["mousedown"] } $ \context -> forever boardV context

sampleData :: TVar (Map Cord Disc) -> IO ()
sampleData boardV = atomically $ do
  board <- readTVar boardV
  writeTVar boardV (fromList [((-4,-4), White),
                              ((-1,-1), Black),
                              ((-1,0), White),
                              ((0,0), Black),
                              ((0,-1), White),
                              ((3,3), Black)])

viewer :: TVar (Map Cord Disc) -> DeviceContext ->IO ()
viewer boardV context = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  board <- atomically $ readTVar boardV
  print board
  send context $ do
    clearRect (0,0, cw, ch)
    beginPath()
    grid (width context) (height context)
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
              | x <- [-4..3::Int]
              , y <- [-4..3::Int]
              ]
    -- print $ (width context, height context)
    return ()
  atomically $ do
    board' <- readTVar boardV
    if (board == board') then retry else return ()
  viewer boardV context

play :: TVar (Map Cord Disc) -> DeviceContext -> Disc -> IO ()
play boardV context turn = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  print $ "waiting for turn: " ++ show turn
  event <- wait context
  --print $ ePageXY event
  let sq = ePageXY event >>= \ (x, y) -> pointToSq (x, y) cw ch

  turn' <- atomically $ do
    board <- readTVar boardV
    case sq of
      Just pos -> case Map.lookup pos board of
                    Nothing ->
                      if (isValidMove board turn)
                      then do writeTVar boardV (Map.insert pos turn board)
                              return $ swap turn
                      else return turn
                      -- already something here
                    Just _ ->  return turn
      Nothing     -> return turn
  play boardV context turn'

forever :: TVar (Map (Int, Int) Disc) -> DeviceContext -> IO ()
forever boardV context = do
        forkIO $ viewer boardV context
        play boardV context White

isValidMove :: Map Cord Disc -> Disc -> Bool
isValidMove board turn = True
