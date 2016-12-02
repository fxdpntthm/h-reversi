{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow               (second)
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad               (when)
import           Data.Map                    (Map, fromList, toList)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Text                   (pack)
import           Debug.Trace
import           Disc
import           Graphics.Blank
import           Grid
import           Util
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
  writeTVar boardV [endGame']
  -- [fromList [((-1,-1), Black),
  --                             ((-1,0), White),
  --                             ((0,0), Black),
  --                             ((0,-1), White)]]

endGame' :: Board
endGame' = fromList [((-4,-4),White),((-4,-3),White),((-4,-2),White),((-4,-1),White),((-4,0),White),((-4,1),White),((-4,2),White),((-3,-4),Black),((-3,-3),Black),((-3,-2),Black),((-3,-1),White),((-3,0),Black),((-3,1),White),((-3,2),White),((-3,3),Black),((-2,-4),Black),((-2,-3),Black),((-2,-2),White),((-2,-1),Black),((-2,0),White),((-2,1),Black),((-2,2),White),((-2,3),Black),((-1,-4),Black),((-1,-3),White),((-1,-2),Black),((-1,-1),Black),((-1,0),White),((-1,1),Black),((-1,2),White),((-1,3),Black),((0,-4),Black),((0,-3),Black),((0,-2),White),((0,-1),Black),((0,0),White),((0,1),White),((0,2),Black),((0,3),Black),((1,-4),Black),((1,-3),White),((1,-2),Black),((1,-1),Black),((1,0),Black),((1,1),Black),((1,2),White),((1,3),Black),((2,-4),Black),((2,-3),Black),((2,-2),White),((2,-1),White),((2,0),Black),((2,1),Black),((2,2),Black),((2,3),Black),((3,-4),Black),((3,-3),Black),((3,-2),Black),((3,-1),Black),((3,0),Black),((3,1),Black),((3,2),Black)]




viewer :: TVar [Board] -> DeviceContext -> Disc ->IO ()
viewer boardV context turn = do
  let (cw, ch, sz) = (width context, height context, min cw ch)

  boardStates <- atomically $ readTVar boardV
  let board = head boardStates
  --print boardStates
  let blacks = length $ filter (\(_,b) -> b == Black) $ Map.toList board
  let whites = length $ filter (\(_,b) -> b == White) $ Map.toList board
  print  (length $ Map.toList board, blacks, whites)
  -- check if valid move exist
  -- TODO Fix this to a proper logic
  let vs = allValidMoves board turn
  send context $ do clearRect (0,0, cw, ch)
                    beginPath()
                    grid (width context) (height context)
                    -- put on all the discs TODO fix this. The ratios aren't proper
                    drawDiscs sz board
                    -- print $ (width context, height context)

  if (not $ null vs)
    then do atomically $ do boardStates' <- readTVar boardV
                            let board' = head boardStates'
                            when (board == board') retry
            viewer boardV context turn
    else endGame context cw ch whites blacks

endGame :: DeviceContext -> Double -> Double -> Int -> Int -> IO ()
endGame context cw ch whites blacks = do {
  send context
    $ do --clearRect( 0,0,cw, ch)
         font "italic 15pt Calibri"
         fillText(pack $ "Game Over! || Final Score || White: "
                   ++ show whites ++ " Black: " ++ show blacks
                   ++ " || Winner: "
                   ++ (show $ if (whites > blacks) then White else Black)
                 , cw/5, ch*0.95)
         return ()
  ; print "Game Over!"}

play :: TVar [Board] -> DeviceContext -> Disc -> IO ()
play boardV context turn = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  boardStates <- atomically $ readTVar boardV
  let board = head boardStates

  let blacks = length $ filter (\(_, b) -> b == Black) $ Map.toList board
  let whites = length $ filter (\(_,b) -> b == White) $ Map.toList board
  -- check if valid move exist
  let vs = allValidMoves board turn
  if null vs
    then viewer boardV context turn
    else do { print board
            ; print $ "waiting for turn: " ++ show turn
            ; event <- wait context
            --print $ ePageXY event
            ; let sq = ePageXY event >>= \ (x, y) -> pointToSq (x, y) cw ch
            ; print sq
            ; turn' <- atomically $ do
                boardStates <- readTVar boardV
                let board = head boardStates

                case sq of
                  Just pos -> case Map.lookup pos board of
                                Nothing ->
                                  if isValidMove pos board turn
                                  then do writeTVar boardV
                                            $ updateBoard pos turn board
                                            : boardStates
                                          return $ swap turn
                                  else return turn
                                -- already something here
                                Just _ ->  return turn
                  Nothing     -> return turn
            ; play boardV context turn' }

forever :: TVar [Board] -> DeviceContext -> Disc -> IO ()
forever boardV context turn = do
        forkIO $ viewer boardV context turn
        play boardV context turn
