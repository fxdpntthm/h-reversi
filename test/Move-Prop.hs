module Main where

import           Data.Map        (Map, fromList)
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import           Disc
import           Grid
import           Test.QuickCheck
main :: IO ()
main = putStrLn "This test always fails!"

sampleBoard :: Board
sampleBoard = fromList [((-1,-1), Black),
                         ((-1,0), White),
                         ((0,0), Black),
                         ((0,-1), White)]

sampleBoard' :: Board
sampleBoard' = fromList [((-1,-1),Black),
                         ((-1,0),White),
                         ((0,-1),White),
                         ((0,0),Black),
                         ((0,1),White),
                         ((1,1),Black)]

sampleBoard'' :: Board
sampleBoard'' = fromList [((-2,-2),Black),((-2,-1),Black),
                          ((-2,0),Black),((-1,-1),Black),
                          ((-1,0),Black),((0,-1),White),
                          ((0,0),Black),((0,1),White),
                          ((1,1),White),((2,1),White)]


endGame :: Board
endGame = fromList [((-4,-4),White),((-4,-3),White),((-4,-2),White),
                    ((-4,-1),White),((-4,0),White),((-4,1),White),
                    ((-4,2),White),((-4,3),White),((-3,-4),White),
                    ((-3,-3),White),((-3,-2),Black),((-3,-1),Black),
                    ((-3,0),Black),((-3,1),Black),((-3,2),Black),
                    ((-3,3),White),((-2,-4),White),((-2,-3),Black),
                    ((-2,-2),White),((-2,-1),Black),((-2,0),Black),
                    ((-2,1),Black),((-2,2),Black),((-2,3),White),
                    ((-1,-4),White),((-1,-3),Black),((-1,-2),Black),
                    ((-1,-1),White),((-1,0),Black),((-1,1),White),
                    ((-1,2),Black),((-1,3),White),((0,-4),White),
                    ((0,-3),White),((0,-2),Black),((0,-1),Black),
                    ((0,0),White),((0,1),Black),((0,2),White),
                    ((0,3),White),((1,-4),White),((1,-3),White),
                    ((1,-2),Black),((1,-1),Black),((1,0),White),
                    ((1,1),Black),((1,2),White),((1,3),White),
                    ((2,-4),White),((2,-3),White),((2,-2),White),
                    ((2,-1),White),((2,0),White),((2,1),White),
                    ((2,2),Black),((2,3),White),((3,-4),Black),
                    ((3,-3),Black),((3,-2),Black),((3,-1),Black),
                    ((3,0),White),((3,1),Black),((3,2),Black)]

endGame' :: Board
endGame' = fromList [((-4,-4),White),((-4,-3),White),((-4,-2),White),((-4,-1),White),((-4,0),White),((-4,1),White),((-4,2),White),((-3,-4),Black),((-3,-3),Black),((-3,-2),Black),((-3,-1),White),((-3,0),Black),((-3,1),White),((-3,2),White),((-3,3),Black),((-2,-4),Black),((-2,-3),Black),((-2,-2),White),((-2,-1),Black),((-2,0),White),((-2,1),Black),((-2,2),White),((-2,3),Black),((-1,-4),Black),((-1,-3),White),((-1,-2),Black),((-1,-1),Black),((-1,0),White),((-1,1),Black),((-1,2),White),((-1,3),Black),((0,-4),Black),((0,-3),Black),((0,-2),White),((0,-1),Black),((0,0),White),((0,1),White),((0,2),Black),((0,3),Black),((1,-4),Black),((1,-3),White),((1,-2),Black),((1,-1),Black),((1,0),Black),((1,1),Black),((1,2),White),((1,3),Black),((2,-4),Black),((2,-3),Black),((2,-2),White),((2,-1),White),((2,0),Black),((2,1),Black),((2,2),Black),((2,3),Black),((3,-4),Black),((3,-3),Black),((3,-2),Black),((3,-1),Black),((3,0),Black),((3,1),Black),((3,2),Black),((3,3),Black)]


validMoveGen :: Board -> Disc -> Gen Cord
validMoveGen board turn = elements $ allValidMoves board turn
-- allValidMoves should be non-empty

-- boardG :: Board -> Disc ->  Board
-- boardG board turn = apply () sampleBoard turn
--  where

-- 1) property --> after every move, total number of discs is increased only by one
prop_discInc pos =
  1 + (length $ Map.toList sampleBoard) == length (Map.toList $ updateBoard pos White sampleBoard)

{-
let {pos = (0,1); board = sampleBoard; turn = White}
let {pos = (2,1); board = sampleBoard'; turn = White}
-}



-- TODO tests
-- How do I generate valid board configurations?
-- What are the properties of valid board configurations?
-- 2) increase in White == decrease in black + 1
-- 3) increase in Black == decrease in white + 1
