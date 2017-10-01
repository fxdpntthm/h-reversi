{-# LANGUAGE MultiParamTypeClasses #-}
module AI where

import           Game.Disc
import           Game.Grid
import           Game.Util
import           System.Random

-- | Returns the next best move
-- this currently usues greedy approach
-- with the heuristic of maximum increase of self discs
-- and minimize the opponents discs
-- and the tree is pruned after 5 levels

data BestGuess = Node Board [Board] | Leaf Board deriving (Eq, Show)

bestNextMove :: Board -> Disc -> Cord
bestNextMove = undefined

-- | A player is an agent that suggests a next move
class Player where
  nextMove :: Board -> Disc -> IO Cord

instance Player Monkey where
  nextMove board turn = undefined
