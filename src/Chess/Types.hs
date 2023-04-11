-- Chess board description
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Types where

type Board = [[Square]]

type Position = (Rank, File)
type Rank     = Int
type File     = Int

data Square = Empty
            | Occupied Color Piece

data Color = Black
           | White

data Piece = Pawn
           | Rook
           | Knight
           | Bishop
           | Queen
           | King

data Direction = Up
               | UpRight
               | Right
               | DownRight
               | Down
               | DownLeft
               | Left
               | UpLeft

-- Is the given position occupied by a piece of the given color?
occupiedBy :: Color -> Position -> Board -> Bool
occupiedBy color pos@(rank, file) brd =
  validPos pos && case brd !! rank !! file of
    Empty          -> False
    Occupied clr _ -> clr == color

-- Is the given position valid?
validPos :: Position -> Bool
validPos (rank, file) = ! (rank < 0 || rank > 7 || file < 0 || file > 7)

otherColor :: Color -> Color
otherColor White = Black
otherColor Black = White

allPos :: [Position]
allPos = [ (rank, file)
         | rank <- [0..7]
         | file <- [0..7]
         ]
