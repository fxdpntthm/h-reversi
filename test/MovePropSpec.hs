module Main (main, player_progress_spec, disc_inc_spec) where

import qualified Data.Map        as Map
import           Data.Set        ()
import           Game.Disc
import           Game.Grid
import           Generators
import           Test.Hspec
import           Test.QuickCheck


main :: IO ()
main = hspec $
       do player_progress_spec
          disc_inc_spec

-- 2) Property --> after every move # of discs of the player who played
-- is greater than previous board state
prop_player_progress :: Cord -> Board -> Disc -> Bool
prop_player_progress pos board turn = (length $ filter (\(_,b) -> b == turn) (Map.toList board))
      < (length $ filter (\(_,b) -> b == turn) (Map.toList $ updateBoard pos turn board))

player_progress_spec :: Spec
player_progress_spec = describe "Player progress spec"
  $ do it "The player who plays should always increment his discs "
  $ property $ forAll arbitraryGameState
             $ \(turn, board, pos) -> prop_player_progress pos board turn

prop_disc_inc :: Cord -> Board -> Disc -> Bool
prop_disc_inc pos board disc =
  1 + (length $ Map.toList board)
  == length (Map.toList $ updateBoard pos disc board)

disc_inc_spec :: Spec
disc_inc_spec = describe "Disc Increment Property" $ do
  it "Total increase in no. of discs should be equal to one" $
       property $ forAll arbitraryGameState
                $ \(turn, board, cord) -> prop_disc_inc cord board turn

-- arbitraryGameState :: Gen (Disc, Board, Cord)
-- arbitraryGameState = do (t, b) <- arbitraryValidBoard
--                         vm <- validMoveGen b t
--                         return (t, b, vm)

-- stepGen :: Board -> Disc -> Gen Board
-- stepGen board turn = do vm <- validMoveGen board turn
--                         return $ updateBoard vm turn board

-- boardGen :: Int -> Board -> Disc -> Gen (Disc, Board)
-- boardGen 0 board turn = return (turn, board)
-- boardGen n board turn = do nb <- stepGen board turn
--                            boardGen (n-1) nb (swap turn)

-- arbitraryValidBoard :: Gen (Disc, Board)
-- arbitraryValidBoard = do t <- choose (0,45)
--                          boardGen t sampleBoard White


-- validMoveGen :: Board -> Disc -> Gen Cord
-- validMoveGen board turn = elements $ allValidMoves board turn

-- 1) property --> after every move, total number of discs is increased only by one

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

