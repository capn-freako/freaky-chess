-- Chess game play mechanics.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 11, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Play where

import Control.Monad.State.Lazy
import Data.List (sortOn)
import Data.Maybe (fromJust)

import Chess.Types
import Chess.Moves

-- attemptMove :: Player -> Board -> Position -> Position -> Either String Board
-- attemptMove clr brd from to = Left "Not implemented, yet."

rankMoves :: Player -> Board -> [Board]
rankMoves clr brd = sortOn rankBoard $ concatMap (movesFromSquare clr brd) allPos

rankBoard :: Board -> Int
rankBoard brd = 0

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
