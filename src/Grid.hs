{-# LANGUAGE OverloadedStrings #-}

module Grid where

import           Control.Applicative
import           Control.Arrow
import           Data.List.Split
import           Data.Map            (Map, fromList)
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Debug.Trace
import           Disc
import           Graphics.Blank
import           Util

-- | Coordinate system goes from -4 to 3
type Cord = (Int, Int)

type Board = Map Cord Disc

-- | Orientation of the line
-- whether it is North, south east, west, south-east, etc
-- The order is important as it matches with the adjacent square list
data Direction = NW | N | NE | E | SE | S | SW | W
  deriving (Show, Eq, Enum)

grid w h = do
        let sz = min w h
        let sqSize = sz / 9
        clearRect (0,0,w,h)
        beginPath()
        save()
        translate (w / 2, h / 2)
        lineWidth 3
        beginPath()
        strokeStyle "black"
        sequence_ $ computeSquare (-sz/2, -sz/2) sqSize <$> gridCord 8
        fillStyle "green"
        fill()
        stroke()
        restore()

gridCord n = [(x,y) | x <- [0..n-1], y <- [0..n-1]]

computeSquare (x0, y0) sz (x, y) = sqr (x0 + x*sz, y0 + y * sz, sz)
sqr (x, y, s) = rect (x, y, s, s)

-- | Returns the square co-ordiantes of the click
pointToSq :: (Double, Double)  -> Double -> Double -> Maybe Cord
pointToSq (x,y) w h = validate $
  do x' <- Just $ round $ ((x - w / 2) / sz) * 10
     y' <- Just $ round $ ((y - h / 2) / sz) * 10
     return (x', y')
  where sz = min w h

-- | validate if the coordinate is inside the board
validate :: Maybe Cord -> Maybe Cord
validate c@(Just (x , y)) = if (x > maxX || x < minX) || (y > maxY || y < minY)
  then Nothing else c
validate Nothing = Nothing

-- | return the adjacent co-ordinates starting from NE clockwise
adjacent :: Cord -> [Cord]
adjacent (x, y) = Prelude.filter (\(a,b) -> a >= minX && a <= maxX
                                   && b >= minY && b <= maxY && (a,b) /= (x,y))
  $ (,) <$> [ x-1..x+1 ] <*> [ y-1..y+1 ]

direction :: Cord -> Cord -> Direction
direction (nc_x, nc_y) (oc_x, oc_y)
  | (nc_x > oc_x) && (nc_y > oc_y) = NW
  | (nc_x == oc_x) && (nc_y > oc_y) = N
  | (nc_x < oc_x) && (nc_y > oc_y) = NE
  | (nc_x < oc_x) && (nc_y == oc_y) = E
  | (nc_x < oc_x) && (nc_y < oc_y) = SE
  | (nc_x == oc_x) && (nc_y < oc_y) = S
  | (nc_x > oc_x) && (nc_y < oc_y) = SW
  | (nc_x > oc_x) && (nc_y == oc_y) = W

-- | Gives the next co-ordinate in the given direction
move :: Direction -> Cord -> Maybe Cord
move N (x,y)  = validate $ return (x, y-1)
move NE (x,y) = validate $ return (x+1,y-1)
move E (x,y)  = validate $ return (x+1,y)
move SE (x,y) = validate $ return (x+1,y+1)
move S (x,y)  = validate $ return (x,y+1)
move SW (x,y) = validate $ return (x-1,y+1)
move W (x,y)  = validate $ return (x-1,y)
move NW (x,y) = validate $ return (x-1,y-1)

-- | It is a valid move if
-- 1) The current pos is empty
-- 2) There is an adjacent square with opposite colored disc
-- 3) placing the disc creates a sandwich
isValidMove :: Cord -> Map Cord Disc -> Disc -> Bool
isValidMove pos board turn = isEmptySquare pos board
  && areAdjacentSquareOpposite pos board turn
  && sandwiches pos board turn

-- | Condition 1) in @isValidMove@
isEmptySquare :: Cord -> Map Cord Disc -> Bool
isEmptySquare pos board = isNothing $ Map.lookup pos board

-- | Condition 2) in @isValidMove@
areAdjacentSquareOpposite :: Cord -> Map Cord Disc -> Disc -> Bool
areAdjacentSquareOpposite pos board turn = not . null
  $ adjacentOppositeSquares pos board turn

-- | All the squares that are adjacent to the current square and have opposite
-- colored disc
adjacentOppositeSquares :: Cord -> Map Cord Disc -> Disc -> [Maybe Disc]
adjacentOppositeSquares  pos board turn =
  filter (== (Just $ swap turn))
  $ flip Map.lookup board <$>  adjacent pos

-- | condition 3) in @isValidMove@
-- Select all adjacent squares that have opposite disc
-- For each of those discs get first disk of same color in appropriate direction
-- if any of such discs exist return True
-- else return False
sandwiches :: Cord -> Map Cord Disc -> Disc -> Bool
sandwiches pos board turn = not . null $ filter isJust
  $  allFirstSameDiscs pos board turn

allFirstSameDiscs pos board turn = sds <$> vps
  where
    l d = move d pos
    ps = zip allDirections (l <$> allDirections)
    vps = filter (\(a, Just b) -> isJust (Map.lookup b board)
                 && (Map.lookup b board /= Just turn))
          $ filter (isJust . snd)
          $ second validate <$> ps
    sds (d, Just p) = getFirstSameDisc p d board turn
    -- z = zip3 allDirections
    --  $ (l <$> allDirections)
    --  $ ((flip Map.lookup board =<<) <$> (l <$> allDirections))

-- | returns the co-ordinate of the first disc of the same color
-- that appears after 1 or more opposite colored discs
getFirstSameDisc :: Cord -> Direction -> Map Cord Disc -> Disc -> Maybe (Cord, Disc)
getFirstSameDisc pos dir board turn = collapse $ head z
  where
    -- get the series of all the coordinates in the given direction
    l = line pos dir
    md = (flip Map.lookup board =<<) <$> l
    z =  dropWhile (\(a,b) -> (b == (Just $ swap turn)))
      $ safeTail
      $ zip l md

updateBoard :: Cord -> Disc -> Board -> Board
updateBoard pos turn board = Map.union (fromList nv) board
  where
    z :: [(Direction, Maybe (Cord, Disc))]
    z = zip allDirections $ allFirstSameDiscs pos board turn
    bs = sequence $ concat $ between pos <$> z
    nv = case bs of
      Just l  ->  zip l $ repeat turn
      Nothing -> []

-- | returns the sequence of squares from fist position to second position
-- including the start and end
between :: Cord -> (Direction, Maybe (Cord, Disc)) -> [Maybe Cord]
between _ (_, Nothing)              = []
between pos1 (_, Just (pos2, disc)) =
  takeWhile (/= Just pos2) $ line pos1 $ direction pos1 pos2

-- | returns a sequence of squares from cord in direction
line :: Cord -> Direction -> [Maybe Cord]
line pos d = l
  where
    l = Just pos : scanl (\c _ -> c >>= move d)
                (Just pos >>= move d) l

allDirections :: [Direction]
allDirections = (toEnum <$> [0..7::Int])::[Direction]

-- | get all valid moves
allValidMoves :: Board -> Disc -> [Cord]
allValidMoves board turn = filter iv cs
  where
    cs = emptyCords board
    iv c =  isValidMove c board turn


emptyCords :: Board -> [Cord]
emptyCords board = Set.toList $ Set.difference bs es
  where
    bs = Set.fromList ((,) <$> [minX..maxX] <*> [minY..maxY])
    es = Set.fromList (fst <$> Map.toList board)
