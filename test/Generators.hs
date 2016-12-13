{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generators where

import           Data.Map        (fromList)
import qualified Data.Tuple      as T
import           Game.Disc
import           Game.Grid
import           Test.QuickCheck

newtype GameState = GS (Disc, Board, Cord) deriving (Show, Eq)

-- | Generates an arbitrary valid game state
instance Arbitrary GameState where
  arbitrary = do bs@(BS (t, b)) <- (arbitrary::Gen BoardState)
                 vm <- validMoveGen bs
                 return $ GS (t, b, vm)

-- | Generates an arbitrary valid boad state starting from start point
newtype BoardState = BS (Disc, Board) deriving (Show, Eq)

instance Arbitrary BoardState where
  arbitrary = do
    n <- arbitrary
    boardGen n (BS (White, sampleBoard))

-- | Emulate the steps between 0 and 30 moves
newtype Steps = Step Int deriving (Show, Eq, Enum)

instance Arbitrary Steps where
  arbitrary = Step <$> choose (0, 20)

boardGen :: Steps -> BoardState -> Gen BoardState
boardGen (Step 0) bs = return $ bs
boardGen st bs@(BS (turn, ob)) =
  do vm <- validMoveGen bs
     let nb = updateBoard vm turn ob
     boardGen (pred st) (BS ((swap turn), nb))


-- | This is an unsafe call as elemens can throw an exception if allValidMoves
-- is empty list
validMoveGen :: BoardState -> Gen Cord
validMoveGen (BS bs) = elements $ uncurry allValidMoves (T.swap bs)

sampleBoard :: Board
sampleBoard = fromList [((-1,-1), Black),
                         ((-1,0), White),
                         ((0,0), Black),
                         ((0,-1), White)]

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
