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


validMoveGen :: Board -> Disc -> Gen Cord
validMoveGen board turn = elements $ allValidMoves board turn
-- allValidMoves should be non-empty

-- boardG :: Board -> Disc ->  Board
-- boardG board turn = apply () sampleBoard turn
--  where

-- | emulates the game
apply :: Cord -> Board -> Disc -> Board
apply pos board turn = updateBoard pos turn board

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
