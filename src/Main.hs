{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Data.Map                    (Map, fromList)
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
  print $ "starting canvas"
  blankCanvas 3000 {events = ["mousedown"] } $ \context -> forever boardV context

sampleData :: TVar Board -> IO ()
sampleData boardV = atomically $ do
  board <- readTVar boardV
  writeTVar boardV (fromList [((-1,-1), Black),
                              ((-1,0), White),
                              ((0,0), Black),
                              ((0,-1), White)])

viewer :: TVar Board -> DeviceContext ->IO ()
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

play :: TVar Board -> DeviceContext -> Disc -> IO ()
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
                      if (isValidMove pos board turn)
                      then do writeTVar boardV (Map.insert pos turn board)
                              return $ swap turn
                      else return turn
                      -- already something here
                    Just _ ->  return turn
      Nothing     -> return turn
  play boardV context turn'

forever :: TVar Board -> DeviceContext -> IO ()
forever boardV context = do
        forkIO $ viewer boardV context
        play boardV context White
-- | It is a valid move if
-- 1) The current pos is empty
-- 2) There is an adjacent square with opposite colored disc
-- 3) placing the disc creates a sandwich
isValidMove :: Cord -> Map Cord Disc -> Disc -> Bool
isValidMove pos board turn = isEmptySquare pos board
  && isAdjacentSquareOpposite pos board turn

isAdjacentSquareOpposite :: Cord -> Map Cord Disc -> Disc -> Bool
isAdjacentSquareOpposite pos board turn = not . null $
  filter (\e -> e /= Nothing && (e == (Just $ swap turn)))
  $ fmap ((flip Map.lookup) sampleBoard)
  $ adjacent pos

isEmptySquare :: Cord -> Map Cord Disc -> Bool
isEmptySquare pos board = (Map.lookup pos board) == Nothing


sampleBoard :: Map Cord Disc
sampleBoard = fromList [((-1,-1), Black),
                              ((-1,0), White),
                              ((0,0), Black),
                              ((0,-1), White)]
