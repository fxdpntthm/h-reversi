{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad               (when)
import           Data.Map                    (Map, fromList, toList)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Text                   (pack)
import           Debug.Trace
import           Game.Disc
import           Game.Grid
import           Game.Util
import           Graphics.Blank

type BoardStateTrail = TVar [(Disc, Board)] 

opts = 3000

main :: IO ()
main = do
  boardV <- newTVarIO []
  -- generate some static data for rendering
  startData boardV
  print $ "starting canvas on port: " ++ show (port opts)
  blankCanvas opts {events = ["mousedown"] }
    $ \context -> forever boardV context

startData :: BoardStateTrail -> IO ()
startData boardV = atomically $ do
  board <- readTVar boardV
  writeTVar boardV [(White, Map.fromList [((-1, -1), Black),
                                          ((-1,  0), White),
                                          ((0,   0), Black),
                                          ((0,  -1), White)])]


viewer :: DeviceContext -> BoardStateTrail -> IO ()
viewer context boardV = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  boardStates <- atomically $ readTVar boardV
  let (turn, board) = head boardStates

  -- print boardStates
  let blacks = length $ filter (isBlack . snd) $ Map.toList board
  let whites = length $ filter (isWhite . snd) $ Map.toList board
  print  (length $ Map.toList board, blacks, whites)

  -- check if valid move exist
  let vs = allValidMoves board turn
  let vs' = allValidMoves board $ swap turn
  send context $ do clearRect (0,0, cw, ch)
                    beginPath()
                    grid (width context) (height context)
                    drawDiscs sz board
                    -- print $ (width context, height context)
                    printTurn context cw ch turn whites blacks
                    save ()

  if not $ null vs
  then do atomically $ do boardStates' <- readTVar boardV
                          let board' = snd $ head boardStates'
                          when (board == board') retry
          viewer context boardV 
  else endGame context cw ch whites blacks

printTurn :: DeviceContext -> Double -> Double -> Disc -> Int -> Int -> Canvas ()
printTurn context cw ch turn whites blacks =
  do clearRect (cw/8, ch*0.95, cw, ch)
     font "italic 15pt Calibri"
     fillText(pack $ "Turn: " ++ show turn ++ " || Score || White: "
                   ++ show whites ++ " Black: " ++ show blacks
             , cw/8, ch*0.95)
     save ()

endGame :: DeviceContext -> Double -> Double -> Int -> Int -> IO ()
endGame context cw ch whites blacks = do
  send context
    $ do clearRect (cw/9, ch*0.90, cw, ch)
         font "italic 15pt Calibri"
         fillText(pack $ "Game Over! || Final Score || White: "
                   ++ show whites ++ " Black: " ++ show blacks
                   ++ " || Winner: "
                   ++ (if whites > blacks
                       then show White
                       else if whites == blacks
                            then "Draw"
                            else show Black)
                 , cw/8, ch*0.95)
         save ()
  print "Game Over!"

play :: DeviceContext -> BoardStateTrail  -> IO ()
play context boardV = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  boardStates <- atomically $ readTVar boardV
  let (turn, board) = head boardStates

  let blacks = length $ filter (isBlack . snd) $ Map.toList board
  let whites = length $ filter (isWhite . snd) $ Map.toList board
  -- check if valid move exist
  let vs = allValidMoves board turn
  let vs' = allValidMoves board $ swap turn

  if null vs
  then if null vs'
       then  viewer context boardV
       else do print $ show turn ++ " cannot play. swapping.."
               atomically $ do writeTVar boardV
                                   $ (swap turn, board) : boardStates
                               return ()
               play context boardV
  else do print board
          print $ "waiting for turn: " ++ show turn
          event <- wait context
          --print $ ePageXY event
          let sq = ePageXY event >>= \ (x, y) -> pointToSq (x, y) cw ch
          print sq
          turn' <- atomically $ do
                boardStates <- readTVar boardV
                let board = snd $ head boardStates

                case sq of
                  Just pos -> case Map.lookup pos board of
                                Nothing ->
                                  if isValidMove pos board turn
                                  then do writeTVar boardV
                                            $ (swap turn, updateBoard pos turn board)
                                            : boardStates
                                          return $ swap turn
                                  else return turn
                                -- already something here
                                Just _ ->  return turn
                  Nothing     -> return turn
          play context boardV

forever :: BoardStateTrail -> DeviceContext -> IO ()
forever boardV context= do
        forkIO $ viewer context boardV
        play context boardV

