-- Chess board description
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Types
  ( Position, pattern Position, mkPosition, mkPositions, validPosition
  , Square (..), getSquare, setSquare, Board (..), isPawn
  , Player (..), Piece (..), Direction (..), diagDirs, rectDirs, allDirs
  , occupied, occupiedBy, otherColor
  , newGame, allPos, printBoard
  ) where

import Control.Monad.State.Lazy
import Data.Char                 (chr)
import Data.HashMap.Strict       (HashMap, fromList)
import Data.Maybe                (mapMaybe)
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
        get >>= (put . setSquare (UnsafePosition pos) square)
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
    ] ++ map (\f -> ((1, f), Occupied Wht P)) [0..7] ++
    [ ((7,0), Occupied Blk R)
    , ((7,1), Occupied Blk N)
    , ((7,2), Occupied Blk B)
    , ((7,3), Occupied Blk Q)
    , ((7,4), Occupied Blk K)
    , ((7,5), Occupied Blk B)
    , ((7,6), Occupied Blk N)
    , ((7,7), Occupied Blk R)
    ] ++ map (\f -> ((6, f), Occupied Blk P)) [0..7]

-- "Smart constructor" idiom moves validation from point of use to
-- point of creation, reducing redundant work.
newtype Position = UnsafePosition (Int, Int)
  deriving newtype (Show, Eq)

pattern Position :: Int -> Int -> Position
pattern Position rank file <- UnsafePosition (rank, file)

{-# COMPLETE Position #-}

-- Client code's only mechanism for making a `Position`.
-- Note the `Maybe`, while the pattern synonym above requires a plain `Position`.
-- This forces validation/filtration to the point of creation,
-- under penalty of type checking failure.
mkPosition :: (Int, Int) -> Maybe Position
mkPosition rankAndFile =
  if validPosition rankAndFile
    then Just (UnsafePosition rankAndFile)
    else Nothing

-- Turn only the valid elements of a list of coordinates into `Position`s.
mkPositions :: [(Int, Int)] -> [Position]
mkPositions = mapMaybe mkPosition

validPosition :: (Int, Int) -> Bool
validPosition (r, f) = not (r < 0 || r > 7 || f < 0 || f > 7)

-- posValid :: Position -> Bool
-- posValid (rank, file) = not (rank < 0 || rank > 7 || file < 0 || file > 7)

data Square = Empty
            | Occupied Player Piece

instance Show Square where
  show Empty                = "   "
  show (Occupied _ piece) = " " ++ show piece ++ " "

getSquare :: Position -> Board -> Square
getSquare (Position rank file) brd = squares brd !! rank !! file

setSquare :: Position -> Square -> Board -> Board
setSquare (Position rank file) square brd =
  brd { squares = replace rank
                          ( replace file
                                    square
                                    (squares brd !! rank)
                          )
                          (squares brd)
      }
 where
  replace :: Int -> a -> [a] -> [a]
  replace ix x xs = take ix xs ++ x : drop (ix + 1) xs

isPawn :: Board -> Position -> Bool
isPawn brd pos = case getSquare pos brd of
  Occupied _ P -> True
  _            -> False

data Player = Blk
            | Wht
  deriving(Eq)

data Piece = P
           | R
           | N
           | B
           | Q
           | K

instance Show Piece where
  show piece =
    let val = case piece of
                P -> 9823
                B -> 9821
                N -> 9822
                R -> 9820
                Q -> 9819
                K -> 9818
     in [chr val]

-- showPiece :: Player -> Piece -> String
-- showPiece clr piece =
--   let val = case piece of
--               P -> 9817
--               B -> 9815
--               N -> 9816
--               R -> 9814
--               Q -> 9813
--               K -> 9812
--    in if clr == Wht
--         then [chr val]
--         else [chr (val + 6)]

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
occupiedBy brd (Position rank file) color =
  -- validPos pos && case squares brd !! rank !! file of
  case squares brd !! rank !! file of
    Empty          -> False
    Occupied clr _ -> clr == color

occupied :: Board -> Position -> Bool
occupied brd pos = occupiedBy' Wht || occupiedBy' Blk
 where
  occupiedBy' = occupiedBy brd pos

-- Is the given position valid?
-- validPos :: Position -> Bool
-- validPos (rank, file) = not (rank < 0 || rank > 7 || file < 0 || file > 7)

otherColor :: Player -> Player
otherColor Wht = Blk
otherColor Blk = Wht

allPos :: [Position]
allPos = [ UnsafePosition (rank, file)
         | rank <- [0..7]
         , file <- [0..7]
         ]

printBoard :: Board -> IO ()
printBoard brd = do
  forM_ (reverse $ zip [0..7] (squares brd)) $ \rank -> do
    putStr $ show $ fst rank + 1
    putStr " "
    _ <- printRank rank
    setSGR [Reset]
    putStrLn ""
  putStrLn "   a  b  c  d  e  f  g  h"

printRank :: (Int, [Square]) -> IO [()]
printRank (rank, sqrs) =
  forM (zip [0..7] sqrs) $ \(file, square) -> do
    (if ((rank + file) `mod` 2) /= 1
      then setSGR [SetColor Background Vivid  Black]
      else setSGR [SetColor Background Dull White])
    (case square of
      Occupied Wht _ -> setSGR [SetColor Foreground Vivid White]
      _              -> setSGR [SetColor Foreground Dull  Black])
    putStr $ show square
