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
    $ \context -> forever boardV context

startData :: TVar [(Disc, Board)] -> IO ()
startData boardV = atomically $ do
  board <- readTVar boardV
  writeTVar boardV [(White, fromList [((-1,-1), Black),
                                      ((-1,0), White),
                                      ((0,0), Black),
                                      ((0,-1), White)])]

endData :: [(Disc, Board)]
endData  = [(Black, fromList [((-4,-4),Black),((-4,-3),Black),((-4,-2),Black),((-4,-1),Black),((-4,0),Black),((-4,1),White),((-3,-4),White),((-3,-3),White),((-3,-2),White),((-3,-1),Black),((-3,0),White),((-3,1),White),((-3,2),White),((-3,3),Black),((-2,-4),White),((-2,-3),White),((-2,-2),Black),((-2,-1),White),((-2,0),Black),((-2,1),White),((-2,2),Black),((-2,3),Black),((-1,-4),White),((-1,-3),Black),((-1,-2),White),((-1,-1),Black),((-1,0),White),((-1,1),Black),((-1,2),White),((-1,3),Black),((0,-4),White),((0,-3),White),((0,-2),Black),((0,-1),Black),((0,0),Black),((0,1),White),((0,2),Black),((0,3),Black),((1,-4),White),((1,-3),Black),((1,-2),White),((1,-1),Black),((1,0),White),((1,1),Black),((1,2),White),((1,3),Black),((2,-4),White),((2,-3),Black),((2,-2),Black),((2,-1),White),((2,0),Black),((2,1),White),((2,2),White),((2,3),Black),((3,-4),White),((3,-3),Black),((3,-2),Black),((3,-1),Black),((3,0),Black),((3,1),Black),((3,2),Black),((3,3),Black)])]


viewer :: TVar [(Disc, Board)] -> DeviceContext ->IO ()
viewer boardV context = do
  let (cw, ch, sz) = (width context, height context, min cw ch)
  boardStates <- atomically $ readTVar boardV
  let (turn, board) = head boardStates

  --print boardStates
  let blacks = length $ filter (isBlack . snd) $ Map.toList board
  let whites = length $ filter (isWhite . snd) $ Map.toList board
  print  (length $ Map.toList board, blacks, whites)
  -- check if valid move exist
  -- TODO Fix this to a proper logic
  let vs = allValidMoves board turn
  let vs' = allValidMoves board $ swap turn
  send context $ do clearRect (0,0, cw, ch)
                    beginPath()
                    grid (width context) (height context)
                    -- put on all the discs TODO fix this. The ratios aren't proper
                    drawDiscs sz board
                    -- print $ (width context, height context)
                    printTurn context cw ch turn whites blacks
                    save ()

  if (not $ null vs)
  then do atomically $ do boardStates' <- readTVar boardV
                          let board' = snd $ head boardStates'
                          when (board == board') retry
          viewer boardV context
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

play :: TVar [(Disc, Board)] -> DeviceContext -> IO ()
play boardV context = do
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
       then  viewer boardV context
       else do print $ show turn ++ " cannot play. swapping.."
               atomically $ do writeTVar boardV
                                   $ (swap turn, board) : boardStates
                               return ()
               play boardV context
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
          play boardV context

forever :: TVar [(Disc, Board)] -> DeviceContext -> IO ()
forever boardV context= do
        forkIO $ viewer boardV context
        play boardV context
