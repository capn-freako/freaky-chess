-- Chess game play mechanics.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 11, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Play where

attemptMove :: Color -> Board -> Position -> Position -> Either Str Board
attemptMove clr brd from to = Left "Not implemented, yet."

rankMoves :: Color -> Board -> [Board]
rankMoves clr brd = sort rankBoard $ concatMap (movesFromSquare clr brd) allPos

rankBoard :: Board -> Int
rankBoard brd = 0
