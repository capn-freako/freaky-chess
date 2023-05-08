-- Chess game play mechanics.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 11, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Chess.Play
Description : Execution of Chess play, both manual and automatic.
Copyright   : (c) David Banas, 2023; all rights reserved World wide.
License     : BSD-3
Maintainer  : capn.freako@gmail.com
Stability   : experimental
Portability : 'stack' LTS 20.17; ASCII

Functions for executing the play of a Chess game from start to finish.
Both manual play, according to typed movement commands, as well as
automatic play, based on N-move pruned look ahead under certain board
evaluatiion heuristics, are supported.
-}
module Chess.Play where

import qualified Data.Vector as V

import Control.Arrow ((>>>))
import Data.Function ((&), on)
import Data.HashMap.Strict ((!))
import Data.List     (maximumBy, minimumBy)
import Data.Maybe    (catMaybes)

import Chess.Types
import Chess.Moves

-- |((new board, future projected score), total # of boards scored)
type ScoredBoard = ((Board, Int), Int)

scoreBoard :: Board -> ScoredBoard
scoreBoard brd = ((brd, rankBoard brd), 1)

-- |Choose best move, based on given look ahead, returning score.
--
-- __Note:__ The returned score corresponds to a board @N@ moves ahead.
bestMove :: Int          -- ^Moves to look ahead. (0 just returns the scored board.)
         -> Player       -- ^The player making this move.
         -> Board        -- ^The existing board.
         -> ScoredBoard
bestMove n clr brd = case n of
  0 -> scoreBoard brd
  _ -> case clr of
    Wht -> (maximumBy (compare `on` snd) scoredBoards, totalMoves)
    Blk -> (minimumBy (compare `on` snd) scoredBoards, totalMoves)
 where
  newBoards    = allMoves clr brd
  f5Rslts      = map (f5 n (-20000) 20000 (otherColor clr) 0) newBoards
  boardScores  = map fst f5Rslts
  totalMoves   = sum $ map snd f5Rslts
  scoredBoards = zip newBoards boardScores

-- |To understand this code, read Mitchel Wand's paper:
-- /Continuation-Based Program Transformation Strategies/.
-- In particular, see Sec. 5.1.
--
-- |The /F5/ function from Sec. 5.1 of Wand's paper.
f5 :: Int         -- ^Moves to look ahead.
   -> Int         -- ^Current minimum score.
   -> Int         -- ^Current maximum score.
   -> Player      -- ^The player making the next move.
   -> Int         -- ^Number of boards scored.
   -> Board       -- ^The board to assess.
   -> (Int, Int)  -- ^(future score, # of boards scored)
f5 n alpha beta clr m brd = case n of
  0 -> leafRslts
  _ -> case newBoards of
    [] -> leafRslts
    _  -> g5 n alpha beta clr m newBoards
 where
  newBoards = allMoves clr brd
  leafRslts = (max alpha (min beta (rankBoard brd)), m+1)

-- |The /G5/ function from Sec. 5.1 of Wand's paper.
g5 :: Int         -- ^Moves to look ahead.
   -> Int         -- ^Current minimum score.
   -> Int         -- ^Current maximum score.
   -> Player      -- ^The player making this move.
   -> Int         -- ^Number of boards scored.
   -> [Board]     -- ^The possible next boards.
   -> (Int, Int)  -- ^(future score, # of boards scored)
g5 n alpha beta clr m = \case
  [] -> error "Whoops! `g5` called with an empty list of next possible boards."
  brd : brds ->
    let (score, m') = f5 (n-1) alpha beta (otherColor clr) m brd
     in case brds of
          [] -> (score, m')
          _  -> h5 n alpha beta clr m' brds score

-- |The /H5/ function from Sec. 5.1 of Wand's paper.
h5 :: Int         -- ^Moves to look ahead.
   -> Int         -- ^Current minimum score.
   -> Int         -- ^Current maximum score.
   -> Player      -- ^The player making this move.
   -> Int         -- ^Number of boards scored.
   -> [Board]     -- ^The possible next boards.
   -> Int         -- ^The current next best.
   -> (Int, Int)  -- ^(future score, # of boards scored)
h5 n alpha beta clr m brds score = case clr of
  Wht -> if score >= beta
           then (score, m)
           else g5 n score beta clr m brds
  Blk -> if score <= alpha
           then (score, m)
           else g5 n alpha score clr m brds

-- |List of new boards corresponding to all possible moves by the given player.
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

-- |Total board score as: white score - black score, according to a particular heuristic.
tally :: PlayerScore -> BoardScore
tally f brd = f Wht brd - f Blk brd

-- |Score the board.
rankBoard :: BoardScore
rankBoard brd = material brd + mobility brd + center brd + pawnStructure brd

-- * Board Scoring Heuristics

{- $heuristics

The following board evaluation heuristics all follow the same pattern
of using the `tally` function, above, in conjunction with a helper
function, which is player-specific.
These helper functions may be written in one of two styles:

    * "right-to-left" - Using this style, which tends to be more common
        among programmers and mathematicians, a composite function grows
        outward, as we move from right to left in the line of code
        defining it. So, for instance, the `mobilityByPlayer` function:

        * first applies the `positionsByPlayer` function to its arguments,
        * then maps the composite function: `length . validNewPos brd`,
          over those results, and
        * finally, sums the result of that mapping.

    * "left-to-right" - Using this style, we do the opposite: send the
        arguments into the left side of a "function chain", gathering
        the final output at the right end of the line of code.
        (See the `pawnStructureByPlayer` function, for an examle of this style.)
        This may be a more intuitive way to write heuristics for those
        not from a programming or mathematical background.

Note that this style choice is a syntactical one only.
Either style may be used to achieve precisely the same computation.
-}

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

-- |Reachable distance from position in given direction.
reachLen :: Board -> Player -> Position -> Direction -> Int
reachLen brd clr pos dir = reachCount 0 brd clr positions
 where
  positions = case getSquare pos brd of
    Occupied _ K -> take 1 poss  -- King can only move one square in any direction.
    _            -> poss
  poss = directionSpan pos dir

reachCount :: Int -> Board -> Player -> [Position] -> Int
reachCount n _   _   []     = n
reachCount n brd clr (p:ps) = case getSquare p brd of
  Occupied clr' _ -> if clr' == clr then n else n+1
  Empty -> reachCount (n+1) brd clr ps

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
  not (any (brd.moved !) pieces)
  && not (inCheck clr brd)                                         -- Can't castle out of check.
  && all (both ((== Empty) . flip getSquare brd)                   -- Can't castle through other pieces.
               (not . flip elem (coveredBy (otherColor clr) brd))  -- Can't castle through check.
         ) (mkPositions locs)

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both p q x = p x && q x
