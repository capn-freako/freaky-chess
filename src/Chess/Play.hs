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

-- attemptMove :: Color -> Board -> Position -> Position -> Either String Board
-- attemptMove clr brd from to = Left "Not implemented, yet."

rankMoves :: Color -> Board -> [Board]
rankMoves clr brd = sortOn rankBoard $ concatMap (movesFromSquare clr brd) allPos

rankBoard :: Board -> Int
rankBoard brd = 0

newGame :: Board
newGame = flip execState emptyBoard $
  forM initialPlacements $
       \(pos, square) -> get >>= (put . fromJust . setSquare pos square)
 where
  initialPlacements =
    [ ((0,0), Occupied White Rook)
    , ((0,1), Occupied White Knight)
    , ((0,2), Occupied White Bishop)
    , ((0,3), Occupied White Queen)
    , ((0,4), Occupied White King)
    , ((0,5), Occupied White Bishop)
    , ((0,6), Occupied White Knight)
    , ((0,7), Occupied White Rook)
    ] ++ map (\file -> ((1, file), Occupied White Pawn)) [0..7] ++
    [ ((7,0), Occupied Black Rook)
    , ((7,1), Occupied Black Knight)
    , ((7,2), Occupied Black Bishop)
    , ((7,3), Occupied Black Queen)
    , ((7,4), Occupied Black King)
    , ((7,5), Occupied Black Bishop)
    , ((7,6), Occupied Black Knight)
    , ((7,7), Occupied Black Rook)
    ] ++ map (\file -> ((6, file), Occupied Black Pawn)) [0..7]

emptyBoard :: Board
emptyBoard = replicate 8 (replicate 8 Empty)
