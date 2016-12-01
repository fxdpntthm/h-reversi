{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad               (when)
import           Data.Map                    (Map, fromList, toList)
import qualified Data.Map                    as Map
import           Debug.Trace
import           Disc
import           Graphics.Blank
import           Grid

main :: IO ()
main = do
  boardV <- newTVarIO []
  -- generate some static data for rendering
  startData boardV
  print  "starting canvas"
  blankCanvas 3000 {events = ["mousedown"] }
    $ \context -> forever boardV context White

startData :: TVar [Board] -> IO ()
startData boardV = atomically $ do
  board <- readTVar boardV
  writeTVar boardV [fromList [((-1,-1), Black),
                              ((-1,0), White),
                              ((0,0), Black),
                              ((0,-1), White)]]

viewer :: TVar [Board] -> DeviceContext ->IO ()
viewer boardV context = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  boardStates <- atomically $ readTVar boardV
  let board = head boardStates
  --print boardStates

  let blacks = length $ filter (\(_,b) -> b == Black) $ Map.toList board
  let whites = length $ filter (\(_,b) -> b == White) $ Map.toList board
  print $ (length $ Map.toList board, blacks, whites)

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
              | x <- [minX..maxX::Int]
              , y <- [minY..maxY::Int]
              ]
    -- print $ (width context, height context)
    return ()
  atomically $ do
    boardStates' <- readTVar boardV
    let board' = head boardStates'
    if (board == board') then retry else return ()
  viewer boardV context

play :: TVar [Board] -> DeviceContext -> Disc -> IO ()
play boardV context turn = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  boardStates <- atomically $ readTVar boardV
  let board = head boardStates
  print board
  print $ "waiting for turn: " ++ show turn
  event <- wait context
  --print $ ePageXY event
  let sq = ePageXY event >>= \ (x, y) -> pointToSq (x, y) cw ch
  print sq
  turn' <- atomically $ do
    boardStates <- readTVar boardV
    let board = head boardStates
    case sq of
      Just pos -> case Map.lookup pos board of
                    Nothing ->
                      if isValidMove pos board turn
                      then do writeTVar boardV
                                $! updateBoard pos turn board : boardStates
                              return $ swap turn
                      else return turn
                      -- already something here
                    Just _ ->  return turn
      Nothing     -> return turn
  play boardV context turn'

forever :: TVar [Board] -> DeviceContext -> Disc -> IO ()
forever boardV context turn = do
        forkIO $ viewer boardV context
        play boardV context turn
