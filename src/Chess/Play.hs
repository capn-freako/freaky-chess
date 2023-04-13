-- Chess game play mechanics.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 11, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Play where

import Control.Arrow ((&&&))
import Control.Monad.State.Lazy
import Data.List (sortOn)
import Data.Maybe (fromJust)

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
materialByPlayer color brd = sum $ map (total color) $ concat brd
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
isCenter (rank, file) = rank > 2 && rank < 5 && file > 2 && file < 5

positionsByPlayer :: Player -> Board -> [Position]
positionsByPlayer clr brd =
  [ pos
  | pos <- allPos
  , occupiedBy brd pos clr
  ]

-- Return the list of positions covered by a piece.
coveredPos :: Board -> Position -> [Position]
coveredPos brd pos@(rank, file) = case getSquare pos brd of
  Nothing     -> []
  Just square -> case square of
    Empty                -> []
    Occupied color piece -> case piece of
      P -> case color of
        Wht -> [(rank+1, file-1), (rank+1, file+1)]
        Blk -> [(rank-1, file-1), (rank-1, file+1)]
      N -> [ pos'
           | pos' <- [ (rank+1, file-2)
                     , (rank+2, file-1)
                     , (rank+2, file+1)
                     , (rank+1, file+2)
                     , (rank-1, file-2)
                     , (rank-2, file-1)
                     , (rank-2, file+1)
                     , (rank-1, file+2)
                     ]
            , validPos pos'
            ]
      K -> concatMap (take 1) $ reaches color allDirs
      R -> concat             $ reaches color rectDirs
      B -> concat             $ reaches color diagDirs
      Q -> concat             $ reaches color allDirs
 where
  reaches clr = map (reach True brd pos clr)

newGame :: Board
newGame = flip execState emptyBoard $
  forM initialPlacements $
       \(pos, square) -> get >>= (put . fromJust . setSquare pos square)
 where
  initialPlacements =
    [ ((0,0), Occupied Wht R)
    , ((0,1), Occupied Wht N)
    , ((0,2), Occupied Wht B)
    , ((0,3), Occupied Wht Q)
    , ((0,4), Occupied Wht K)
    , ((0,5), Occupied Wht B)
    , ((0,6), Occupied Wht N)
    , ((0,7), Occupied Wht R)
    ] ++ map (\file -> ((1, file), Occupied Wht P)) [0..7] ++
    [ ((7,0), Occupied Blk R)
    , ((7,1), Occupied Blk N)
    , ((7,2), Occupied Blk B)
    , ((7,3), Occupied Blk Q)
    , ((7,4), Occupied Blk K)
    , ((7,5), Occupied Blk B)
    , ((7,6), Occupied Blk N)
    , ((7,7), Occupied Blk R)
    ] ++ map (\file -> ((6, file), Occupied Blk P)) [0..7]

emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Empty)
