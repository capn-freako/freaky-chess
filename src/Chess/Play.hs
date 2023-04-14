-- Chess game play mechanics.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 11, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Play where

import Control.Arrow ((&&&))
import Data.List (sortOn)

import Chess.Types
import Chess.Moves

rankMoves :: Player -> Board -> [(Int, Board)]
rankMoves clr brd =
  let myMoves = allMoves clr brd
      clr'    = otherColor clr
      rslts   = sortOn fst $ map ((bestFor clr' . allMoves clr') &&& id) myMoves
   in if clr == Blk
        then rslts
        else reverse rslts
 where
  bestFor :: Player -> [Board] -> Int
  bestFor Wht = maximum . map rankBoard
  bestFor Blk = minimum . map rankBoard

allMoves :: Player -> Board -> [Board]
allMoves clr brd = concatMap (movesFromSquare clr brd) allPos

type BoardScore  = Board -> Int
type PlayerScore = Player -> BoardScore

tally :: PlayerScore -> BoardScore
tally f brd = f Wht brd - f Blk brd

rankBoard :: BoardScore
rankBoard brd = material brd + mobility brd + center brd

material :: BoardScore
material = tally materialByPlayer

mobility :: BoardScore
mobility = tally mobilityByPlayer

center :: BoardScore
center = tally centerByPlayer

materialByPlayer :: PlayerScore
materialByPlayer color brd = sum $ map (total color) $ concat (squares brd)
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

mobilityByPlayer :: PlayerScore
mobilityByPlayer clr brd =
  sum $ map (length . validNewPos brd) $ positionsByPlayer clr brd

centerByPlayer :: PlayerScore
centerByPlayer clr brd =
  sum $ map (length . filter isCenter . coveredPos brd) $ positionsByPlayer clr brd

isCenter :: Position -> Bool
isCenter (Position rank file) = rank > 2 && rank < 5 && file > 2 && file < 5

-- All board positions occupied by a piece of the given color.
positionsByPlayer :: Player -> Board -> [Position]
positionsByPlayer clr brd =
  [ pos
  | pos <- allPos
  , occupiedBy brd pos clr
  ]

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
