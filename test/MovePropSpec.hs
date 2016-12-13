module Main (main) where

import qualified Data.Map        as Map
import           Data.Set        ()
import           Game.Grid
import           Generators
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $
       do playerProgressSpec
          discIncSpec

-- | Property --> after every move the total number of discs increase by one
prop_disc_inc :: GameState -> Bool
prop_disc_inc (GS (disc, board, pos)) =
  1 + (length $ Map.toList board)
  == length (Map.toList $ updateBoard pos disc board)

discIncSpec :: Spec
discIncSpec = describe "Disc Increment Property"
  $ it "Total increase in no. of discs should be equal to one"
  $ property  $ forAll (arbitrary :: Gen GameState)
              $ \gs -> prop_disc_inc gs

-- | Property --> after every move # of discs of the player who played
-- is greater than previous board state
prop_player_progress :: GameState  -> Bool
prop_player_progress (GS (turn, board, pos)) =
  (length $ filter (\(_,b) -> b == turn) (Map.toList board))
      < (length $ filter (\(_,b) -> b == turn) (Map.toList $ updateBoard pos turn board))

playerProgressSpec :: Spec
playerProgressSpec = describe "Player progress spec"
  $ it "The player who plays always increments his discs "
  $ property $ forAll (arbitrary:: Gen GameState)
             $ \gs -> prop_player_progress gs
