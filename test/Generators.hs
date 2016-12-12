module Generators where

import           Data.Map        (fromList)
import           Game.Disc
import           Game.Grid
import           Test.QuickCheck

-- | Generates an arbitrary valid game state
arbitraryGameState :: Gen (Disc, Board, Cord)
arbitraryGameState = do (t, b) <- arbitraryValidBoard
                        vm <- validMoveGen b t
                        return (t, b, vm)

stepGen :: Board -> Disc -> Gen Board
stepGen board turn = do vm <- validMoveGen board turn
                        return $ updateBoard vm turn board

boardGen :: Int -> Board -> Disc -> Gen (Disc, Board)
boardGen 0 board turn = return (turn, board)
boardGen n board turn = do nb <- stepGen board turn
                           boardGen (n-1) nb (swap turn)

arbitraryValidBoard :: Gen (Disc, Board)
arbitraryValidBoard = do t <- choose (0,30)
                         boardGen t sampleBoard White


validMoveGen :: Board -> Disc -> Gen Cord
validMoveGen board turn = elements $ allValidMoves board turn

sampleBoard :: Board
sampleBoard = fromList [((-1,-1), Black),
                         ((-1,0), White),
                         ((0,0), Black),
                         ((0,-1), White)]

-- sampleBoard' :: Board
-- sampleBoard' = fromList [((-1,-1),Black),
--                          ((-1,0),White),
--                          ((0,-1),White),
--                          ((0,0),Black),
--                          ((0,1),White),
--                          ((1,1),Black)]

-- sampleBoard'' :: Board
-- sampleBoard'' = fromList [((-2,-2),Black),((-2,-1),Black),
--                           ((-2,0),Black),((-1,-1),Black),
--                           ((-1,0),Black),((0,-1),White),
--                           ((0,0),Black),((0,1),White),
--                           ((1,1),White),((2,1),White)]



-- endGame :: Board
-- endGame = fromList [((-4,-4),White),((-4,-3),White),((-4,-2),White),
--                     ((-4,-1),White),((-4,0),White),((-4,1),White),
--                     ((-4,2),White),((-4,3),White),((-3,-4),White),
--                     ((-3,-3),White),((-3,-2),Black),((-3,-1),Black),
--                     ((-3,0),Black),((-3,1),Black),((-3,2),Black),
--                     ((-3,3),White),((-2,-4),White),((-2,-3),Black),
--                     ((-2,-2),White),((-2,-1),Black),((-2,0),Black),
--                     ((-2,1),Black),((-2,2),Black),((-2,3),White),
--                     ((-1,-4),White),((-1,-3),Black),((-1,-2),Black),
--                     ((-1,-1),White),((-1,0),Black),((-1,1),White),
--                     ((-1,2),Black),((-1,3),White),((0,-4),White),
--                     ((0,-3),White),((0,-2),Black),((0,-1),Black),
--                     ((0,0),White),((0,1),Black),((0,2),White),
--                     ((0,3),White),((1,-4),White),((1,-3),White),
--                     ((1,-2),Black),((1,-1),Black),((1,0),White),
--                     ((1,1),Black),((1,2),White),((1,3),White),
--                     ((2,-4),White),((2,-3),White),((2,-2),White),
--                     ((2,-1),White),((2,0),White),((2,1),White),
--                     ((2,2),Black),((2,3),White),((3,-4),Black),
--                     ((3,-3),Black),((3,-2),Black),((3,-1),Black),
--                     ((3,0),White),((3,1),Black),((3,2),Black)]

-- endGame' :: Board
-- endGame' = fromList [((-4,-4),White),((-4,-3),White),((-4,-2),White),((-4,-1),White),((-4,0),White),((-4,1),White),((-4,2),White),((-3,-4),Black),((-3,-3),Black),((-3,-2),Black),((-3,-1),White),((-3,0),Black),((-3,1),White),((-3,2),White),((-3,3),Black),((-2,-4),Black),((-2,-3),Black),((-2,-2),White),((-2,-1),Black),((-2,0),White),((-2,1),Black),((-2,2),White),((-2,3),Black),((-1,-4),Black),((-1,-3),White),((-1,-2),Black),((-1,-1),Black),((-1,0),White),((-1,1),Black),((-1,2),White),((-1,3),Black),((0,-4),Black),((0,-3),Black),((0,-2),White),((0,-1),Black),((0,0),White),((0,1),White),((0,2),Black),((0,3),Black),((1,-4),Black),((1,-3),White),((1,-2),Black),((1,-1),Black),((1,0),Black),((1,1),Black),((1,2),White),((1,3),Black),((2,-4),Black),((2,-3),Black),((2,-2),White),((2,-1),White),((2,0),Black),((2,1),Black),((2,2),Black),((2,3),Black),((3,-4),Black),((3,-3),Black),((3,-2),Black),((3,-1),Black),((3,0),Black),((3,1),Black),((3,2),Black),((3,3),Black)]
