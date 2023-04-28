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
import Data.HashMap.Strict ((!))
import Data.List     (sortOn)
import Data.Maybe    (catMaybes)

import Chess.Types
import Chess.Moves

-- Choose best move, based on given look ahead, returning score.
--
-- Note: The returned score corresponds to a board `n` moves ahead.
bestMove :: Int -> Player -> Board -> ((Int, Board), Int)  -- ((score, board), # of moves tried)
bestMove n clr brd = case n of
  0 -> case (sortFor clr (sortOn fst) $ map (rankBoard &&& id) newBoards) of
    []  -> ((rankBoard brd, brd), 0)
    x:_ -> (x, length newBoards)
  _ -> let nextResults = map ((bestMove (n-1) clr') &&& id) $ prune newBoards
           nMoves      = sum $ map (snd . fst) nextResults
           (futureScore, bestMv) = case sortFor clr sortNextResults nextResults of
             []                                    -> (rankBoard brd, brd)
             (((futureScore', _), _), bestMv') : _ -> (futureScore', bestMv')
        in ((futureScore, bestMv), nMoves)
 where
  sortNextResults :: Ord a => [(((a,b), c), Board)] -> [(((a,b), c), Board)]
  sortNextResults = sortOn (fst . fst . fst)
  sortFor :: Player -> ([a] -> [a]) -> [a] -> [a]
  sortFor Wht sorter = reverse . sorter
  sortFor Blk sorter = sorter
  newBoards :: [Board]
  newBoards = allMoves clr brd
  prune :: [Board] -> [Board]
  prune = map snd . take 10 . sortFor clr (sortOn (fst . fst . fst)) . map (bestMove 0 clr' &&& id)
  clr' = otherColor clr

allMoves :: Player -> Board -> [Board]
allMoves clr brd = filter (not . inCheck clr) (normalMoves ++ catMaybes castleMoves)
 where
  normalMoves = concatMap (movesFromSquare clr brd) $ positionsByPlayer brd clr
  castleMoves =
    [ movePiece (pos !! 0) (pos !! 1) brd >>= movePiece (pos !! 2) (pos !! 3)
    | canCastleRight clr brd
    ] ++
    [ movePiece (pos !! 4) (pos !! 5) brd >>= movePiece (pos !! 6) (pos !! 7)
    | canCastleLeft  clr brd
    ]
  pos = case clr of
    Wht -> mkPositions [ (0,4), (0,6), (0,7), (0,5)  -- White King side
                       , (0,4), (0,2), (0,0), (0,3)  -- White Queen side
                       ]
    Blk -> mkPositions [ (7,4), (7,6), (7,7), (7,5)  -- Black King side
                       , (7,4), (7,2), (7,0), (7,3)  -- Black Queen side
                       ]

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
  Occupied _ N -> length $ validNewPos brd pos
  Occupied _ B -> sum $ map (reachLen brd color pos) diagDirs
  Occupied _ R -> sum $ map (reachLen brd color pos) rectDirs
  Occupied _ Q -> sum $ map (reachLen brd color pos) allDirs
  _            -> 0

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

{-# INLINE isCenter #-}

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

-- Determine whether castling to King side is an option.
canCastleRight :: Player -> Board -> Bool
canCastleRight clr = canCastle pieces locs clr
 where
  (pieces, rnk) = case clr of
    Wht -> (["WK", "WKR"], 0)
    Blk -> (["BK", "BKR"], 7)
  locs = [(rnk,5),(rnk,6)]

-- Determine whether castling to Queen side is an option.
canCastleLeft :: Player -> Board -> Bool
canCastleLeft clr = canCastle pieces locs clr
 where
  (pieces, rnk) = case clr of
    Wht -> (["WK", "WQR"], 0)
    Blk -> (["BK", "BQR"], 7)
  locs = [(rnk,1),(rnk,2),(rnk,3)]

-- Castling helper function (i.e. - factored commonality)
canCastle :: [String] -> [(Int,Int)] -> Player -> Board -> Bool
canCastle pieces locs clr brd =
  not (or (map (brd.moved !) pieces))
  && not (inCheck clr brd)                                           -- Can't castle out of check.
  && all (both ((== Empty) . flip getSquare brd)                     -- Can't castle through other pieces.
               (not . (flip elem $ coveredBy (otherColor clr) brd))  -- Can't castle through check.
         ) (mkPositions locs)

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both p q x = p x && q x
