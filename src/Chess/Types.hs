-- Chess board description
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Types where

import Control.Monad (forM)
import System.Console.ANSI

type Board = [[Square]]

type Position = (Rank, File)
type Rank     = Int
type File     = Int

data Square = Empty
            | Occupied Player Piece

instance Show Square where
  show Empty                = "   "
  show (Occupied clr piece) = " " ++ show piece ++ " "

data Player = Blk
           | Wht
  deriving(Eq)

data Piece = P
           | R
           | N
           | B
           | Q
           | K
  deriving (Show)

data Direction = U
               | NE
               | E
               | SE
               | S
               | SW
               | L
               | NW

diagDirs :: [Direction]
diagDirs = [NW, NE, SE, SW]

rectDirs :: [Direction]
rectDirs = [U, S, E, L]

allDirs :: [Direction]
allDirs  = diagDirs ++ rectDirs

-- Is the given position occupied by a piece of the given color?
occupiedBy :: Board -> Position -> Player -> Bool
occupiedBy brd pos@(rank, file) color =
  validPos pos && case brd !! rank !! file of
    Empty          -> False
    Occupied clr _ -> clr == color

occupied :: Board -> Position -> Bool
occupied brd pos = occupiedBy' Wht || occupiedBy' Blk
 where
  occupiedBy' = occupiedBy brd pos

-- Is the given position valid?
validPos :: Position -> Bool
validPos (rank, file) = not (rank < 0 || rank > 7 || file < 0 || file > 7)

otherColor :: Player -> Player
otherColor Wht = Blk
otherColor Blk = Wht

allPos :: [Position]
allPos = [ (rank, file)
         | rank <- [0..7]
         , file <- [0..7]
         ]

printBoard :: Board -> IO [()]
printBoard brd =
  forM (reverse $ zip [0..7] brd) $ \rank -> do
    printRank rank
    setSGR [Reset]
    putStrLn ""

printRank :: (Int, [Square]) -> IO [()]
printRank (rank, squares) =
  forM (zip [0..7] squares) $ \(file, square) -> do
    (if ((rank + file) `mod` 2) == 1
      then setSGR [SetColor Background Dull  White]
      else setSGR [SetColor Background Vivid Black])
    (case square of
      Occupied Wht _ -> setSGR [SetColor Foreground Vivid White]
      _              -> setSGR [SetColor Foreground Dull  Black])
    putStr $ show square
