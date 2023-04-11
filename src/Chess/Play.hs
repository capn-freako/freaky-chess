-- Chess game play mechanics.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 11, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Play where

import Data.List (sortOn)

import Chess.Types
import Chess.Moves

attemptMove :: Color -> Board -> Position -> Position -> Either String Board
attemptMove clr brd from to = Left "Not implemented, yet."

rankMoves :: Color -> Board -> [Board]
rankMoves clr brd = sortOn rankBoard $ concatMap (movesFromSquare clr brd) allPos

rankBoard :: Board -> Int
rankBoard brd = 0
