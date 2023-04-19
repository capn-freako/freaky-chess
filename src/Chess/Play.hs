-- Chess game play mechanics.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 11, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Play where

import qualified Data.Vector as V

import Control.Arrow ((&&&), (>>>))
import Data.Function ((&))
import Data.List     (sortOn)

import Chess.Types
import Chess.Moves

-- Choose best move, based on given look ahead, returning score.
--
-- Note: The returned score corresponds to a board `n` moves ahead.
bestMove :: Int -> Player -> Board -> (Int, Board)
bestMove n clr =
  let f = case n of
            0 -> rankBoard                             &&& id
            _ -> fst . bestMove (n-1) (otherColor clr) &&& id
   in head . sortFor clr . map f . allMoves clr
 where
  sortFor :: Ord a => Player -> [(a,b)] -> [(a,b)]
  sortFor Wht = reverse . sortOn fst
  sortFor Blk = sortOn fst

-- ToDo: Change `allPos` to just occupied squares.
allMoves :: Player -> Board -> [Board]
allMoves clr brd = concatMap (movesFromSquare clr brd) allPos

type BoardScore  = Board -> Int
type PlayerScore = Player -> BoardScore

tally :: PlayerScore -> BoardScore
tally f brd = f Wht brd - f Blk brd

rankBoard :: BoardScore
rankBoard brd = material brd + mobility brd + center brd + pawnStructure brd

-- The following board evaluation heuristics all follow the same pattern
-- of using the `tally` function, above, in conjunction with a helper
-- function, which is player-specific.
-- These helper functions may be written in one of two styles:
--
-- * "right-to-left" - Using this style, which tends to be more common
--     among programmers and mathematicians, a composite function grows
--     outward, as we move from right to left in the line of code
--     defining it. So, for instance, the `mobilityByPlayer` function:
--
--     * first applies the `positionsByPlayer` function to its arguments,
--     * then maps the composite function: `length . validNewPos brd`,
--       over those results, and
--     * finally, sums the result of that mapping.
--
-- * "left-to-right" - Using this style, we do the opposite: send the
--     arguments into the left side of a "function chain", gathering
--     the final output at the right end of the line of code.
--     (See the `pawnStructureByPlayer` function, for an examle of this style.)
--     This may be a more intuitive way to write heuristics for those
--     not from a programming or mathematical background.
--
-- Note that this style choice is a syntactical one only.
-- Either style may be used to achieve precisely the same computation.

material :: BoardScore
material = tally materialByPlayer

mobility :: BoardScore
mobility = tally mobilityByPlayer

center :: BoardScore
center = tally centerByPlayer

pawnStructure :: BoardScore
pawnStructure = tally pawnStructureByPlayer

-- Uses "right-to-left" style.
materialByPlayer :: PlayerScore
materialByPlayer color brd = V.sum $ V.map (V.sum . V.map (total color)) $ squares brd
 where
  total :: Player -> Square -> Int
  total clr (Occupied clr' piece) | clr' == clr = value piece
                                  | otherwise   = 0
  total _   _                                   = 0
  value :: Piece -> Int
  value P = 100
  value B = 300
  value N = 300
  value R = 500
  value Q = 900
  value K = 10000

-- Uses "right-to-left" style.
mobilityByPlayer :: PlayerScore
mobilityByPlayer clr brd =
  sum $ map (totalSpan brd clr) $ positionsByPlayer brd clr

-- A more efficient mobility calculator.
totalSpan :: Board -> Player -> Position -> Int
totalSpan brd color pos = case getSquare pos brd of
  Occupied _ P -> length $ validNewPos brd pos
  Occupied _ N -> length $ validNewPos brd pos
  Occupied _ B -> sum $ map (reachLen brd color pos) diagDirs
  Occupied _ R -> sum $ map (reachLen brd color pos) rectDirs
  Occupied _ Q -> sum $ map (reachLen brd color pos) allDirs
  Occupied _ K -> sum $ map (reachLen brd color pos) allDirs
  Empty        -> 0

-- Reachable distance from position in given direction.
reachLen :: Board -> Player -> Position -> Direction -> Int
reachLen brd clr pos dir = reachCount 0 brd clr positions
 where
  positions = case getSquare pos brd of
    Occupied _ K -> take 1 poss  -- King can only move one square in any direction.
    _            -> poss
  poss = directionSpan pos dir

reachCount :: Int -> Board -> Player -> [Position] -> Int
reachCount n _   _   []     = n
reachCount n brd clr (p:ps) =
  if occupiedBy brd p clr                      -- Have we bumped into one of our own pieces?
    then n
    else if occupiedBy brd p (otherColor clr)  -- Have we bumped into a piece of the other color?
           then n+1                            -- Ability to capture extends our reach by one square.
           else reachCount (n+1) brd clr ps    -- Still unobstructed; continue counting.

-- Uses "right-to-left" style.
centerByPlayer :: PlayerScore
centerByPlayer clr brd =
  sum $ map (length . filter isCenter . coveredPos brd) $ positionsByPlayer brd clr

isCenter :: Position -> Bool
isCenter (Position rank file) = rank > 2 && rank < 5 && file > 2 && file < 5

-- Note: This heuristic is written in "left-to-right" style.
--
-- This function counts the number of unique diagonally adjacent pawn pairs.
--
-- ToDo: Add penalty for doubled pawns.
pawnStructureByPlayer :: PlayerScore
pawnStructureByPlayer clr brd = clr &
  (positionsByPlayer brd >>> filter (isPawn brd) >>> allPairs >>> filter areDiagAdjacent >>> length)

-- Return all unique pairs of list elements.
allPairs :: [a] -> [(a, a)]
allPairs [] = []
allPairs (x:xs) = map (x,) xs ++ allPairs xs

areDiagAdjacent :: (Position, Position) -> Bool
areDiagAdjacent (Position rank1 file1, Position rank2 file2) =
  abs (rank1 - rank2) == 1 && abs (file1 - file2) == 1

-- All board positions occupied by a piece of the given color.
positionsByPlayer :: Board -> Player -> [Position]
positionsByPlayer brd = \case
  Wht -> brd.occupiedByWht
  Blk -> brd.occupiedByBlk

{-# INLINE positionsByPlayer #-}

-- Return the list of positions covered by a piece.
coveredPos :: Board -> Position -> [Position]
coveredPos brd pos@(Position rank file) = case getSquare pos brd of
  Empty                -> []
  Occupied color piece -> case piece of
    P -> case color of
      Wht -> mkPositions [(rank+1, file-1), (rank+1, file+1)]
      Blk -> mkPositions [(rank-1, file-1), (rank-1, file+1)]
    N -> mkPositions
           [ (rank+1, file-2)
           , (rank+2, file-1)
           , (rank+2, file+1)
           , (rank+1, file+2)
           , (rank-1, file-2)
           , (rank-2, file-1)
           , (rank-2, file+1)
           , (rank-1, file+2)
           ]
    K -> concatMap (take 1) $ reaches color allDirs
    R -> concat             $ reaches color rectDirs
    B -> concat             $ reaches color diagDirs
    Q -> concat             $ reaches color allDirs
 where
  reaches clr = map (reach True brd pos clr)
