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

import Control.Arrow               ((>>>))
import Control.Concurrent.Async    (mapConcurrently)
import Control.Parallel.Strategies (using, parTraversable, rseq, usingIO)
import Data.Function               ((&), on)
import Data.HashMap.Strict         ((!))
import Data.IORef                  (IORef, newIORef, atomicWriteIORef, readIORef, writeIORef)
import Data.List                   (maximumBy, minimumBy)
import Data.Maybe                  (catMaybes)
import System.IO.Unsafe            (unsafePerformIO)

import Chess.Types
import Chess.Moves

-- |((new board, future projected score), total # of boards scored)
type ScoredBoard = ((Board, Int), Int)

scoreBoard :: Board -> ScoredBoard
scoreBoard brd = ((brd, rankBoard brd), 1)

-- Globals
minScore :: Int
minScore = (-20000)

maxScore :: Int
maxScore = 20000

alpha' :: IORef Int
alpha' = unsafePerformIO $ newIORef minScore

beta' :: IORef Int
beta' = unsafePerformIO $ newIORef maxScore

nScored' :: IORef Int
nScored' = unsafePerformIO $ newIORef 0

-- |Choose best move, based on given look ahead, returning score.
--
-- __Note:__ The returned score corresponds to a board @N@ moves ahead.
-- bestMove :: IORef Int    -- ^Min. score.
--          -> IORef Int    -- ^Max. score.
--          -> IORef Int    -- ^# of boards scored.
bestMove :: Int          -- ^Moves to look ahead. (0 just returns the scored board.)
         -> Player       -- ^The player making this move.
         -> Board        -- ^The existing board.
         -> IO ScoredBoard
bestMove n clr brd = case n of
  0 -> return $ scoreBoard brd  -- Should not occur, but is not technically an error.
  _ -> do
    writeIORef alpha' minScore
    writeIORef beta'  maxScore
    writeIORef nScored' 0
    parF5 (n-1) (otherColor clr) $ allMoves clr brd

-- |Parallelized /F5/.
parF5 :: Int      -- ^Moves to look ahead.
      -> Player   -- ^The player making this move.
      -> [Board]  -- ^The list of boards to evaluate in parallel.
      -> IO ScoredBoard
parF5 n clr brds = do
  -- f5Rslts <- mapM (f5 n clr) brds `usingIO` parTraversable rseq
  f5Rslts <- mapConcurrently (f5 n clr) brds
  let boardScores  = map fst f5Rslts
      totalMoves   = sum $ map snd f5Rslts
      scoredBoards = zip brds boardScores
  return $ case clr of
      Wht -> (maximumBy (compare `on` snd) scoredBoards, totalMoves)
      Blk -> (minimumBy (compare `on` snd) scoredBoards, totalMoves)

-- |The /F5/ function from Sec. 5.1 of Wand's paper.
--
-- To understand this code, read Mitchel Wand's paper:
-- /Continuation-Based Program Transformation Strategies/.
-- In particular, see Sec. 5.1.
f5 :: Int            -- ^Moves to look ahead.
   -> Player         -- ^The player making the next move.
   -> Board          -- ^The board to assess.
   -> IO (Int, Int)  -- ^(future score, # of boards scored)
f5 n clr brd = do
  alpha   <- readIORef alpha'
  beta    <- readIORef beta'
  nScored <- readIORef nScored'
  let (score, m) = (max alpha (min beta (rankBoard brd)), nScored+1)
      ret        = do
        case clr of
          Wht -> if score /= alpha  -- Score sets a new best case for White.
                   then writeIORef beta' score
                   else return ()
          Blk -> if score /= beta  -- Score sets a new best case for Black.
                   then writeIORef alpha' score
                   else return ()
        return (score, m)
  case n of
    0 -> ret
    _ -> case newBoards of
      [] -> ret
      _  -> do
        writeIORef nScored' m
        ((_, score'), m') <- parF5 (n-1) (otherColor clr) newBoards
        return (score', m')
 where
  newBoards = allMoves clr brd

-- |The /G5/ function from Sec. 5.1 of Wand's paper.
--
-- __Note:__ Wand's /H5/ function has been pulled into this function.
-- g5 :: Int
--    -> Int
--    -> Int
--    -> Int            -- ^Moves to look ahead.
--    -> Player         -- ^The player making this move.
--    -> [Board]        -- ^The possible next boards.
--    -> IO (Int, Int)  -- ^(future score, # of boards scored)
-- g5 alpha beta nScored n clr = \case
--   [] -> error "Whoops! `g5` called with an empty list of next possible boards."
--   brd : brds -> do
--     (score, m) <- f5 (n-1) (otherColor clr) brd
--     case brds of
--       [] -> return (score, m)
--       _  -> case clr of
--               Wht -> if score >= beta
--                        then return (score, m)
--                        else g5 score beta m n clr brds
--               Blk -> if score <= alpha
--                        then return (score, m)
--                        else g5 alpha score m n clr brds

-- |List of new boards corresponding to all possible moves by the given player.
allMoves :: Player -> Board -> [Board]
allMoves clr brd = filter (not . inCheck clr) (normalMoves ++ catMaybes castleMoves)  -- no gain w/ parMap
 where
  normalMoves = concatMap (movesFromSquare clr brd) $ positionsByPlayer brd clr  -- no gain w/ parMap
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
