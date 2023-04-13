-- Chess board description
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Types where

import Control.Monad.State.Lazy
import Data.HashMap.Strict       (HashMap, fromList)
import Data.Maybe                (fromJust)
import System.Console.ANSI

data Board = Board
  { squares  :: [[Square]]
  , moved    :: HashMap String Bool
  , lastMove :: Maybe Position
  }

newGame :: Board
newGame = execState
  ( forM initialPlacements $
      \(pos, square) ->
        get >>= (put . fromJust . setSquare pos square)
  )
  Board
  { squares = replicate 8 (replicate 8 Empty)
  , moved = fromList [ ("WK",  False)
                     , ("WKR", False)
                     , ("WQR", False)
                     , ("BK",  False)
                     , ("BKR", False)
                     , ("BQR", False)
                     ]
  , lastMove = Nothing
  }
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

type Position = (Rank, File)
type Rank     = Int
type File     = Int

posValid :: Position -> Bool
posValid (rank, file) = not (rank < 0 || rank > 7 || file < 0 || file > 7)

data Square = Empty
            | Occupied Player Piece

instance Show Square where
  show Empty                = "   "
  show (Occupied _ piece) = " " ++ show piece ++ " "

getSquare :: Position -> Board -> Maybe Square
getSquare pos@(rank, file) brd
  | posValid pos = Just (squares brd !! rank !! file)
  | otherwise    = Nothing

setSquare :: Position -> Square -> Board -> Maybe Board
setSquare pos@(rank, file) square brd
  | posValid pos = Just brd { squares = replace rank
                                                ( replace file
                                                          square
                                                          (squares brd !! rank)
                                                )
                                                (squares brd)
                            }
  | otherwise    = Nothing
 where
  replace :: Int -> a -> [a] -> [a]
  replace ix x xs = take ix xs ++ x : drop (ix + 1) xs


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

data Direction = U  -- "N" is used above.
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
  validPos pos && case squares brd !! rank !! file of
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

printBoard :: Board -> IO ()
printBoard brd = do
  forM_ (reverse $ zip [0..7] (squares brd)) $ \rank -> do
    putStr $ show $ fst rank + 1
    putStr " "
    printRank rank
    setSGR [Reset]
    putStrLn ""
  putStrLn "   a  b  c  d  e  f  g  h"

printRank :: (Int, [Square]) -> IO [()]
printRank (rank, sqrs) =
  forM (zip [0..7] sqrs) $ \(file, square) -> do
    (if ((rank + file) `mod` 2) /= 1
      then setSGR [SetColor Background Dull  Cyan]
      else setSGR [SetColor Background Vivid Green])
    (case square of
      Occupied Wht _ -> setSGR [SetColor Foreground Vivid White]
      _              -> setSGR [SetColor Foreground Dull  Black])
    putStr $ show square
