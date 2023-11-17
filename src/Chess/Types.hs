-- Chess board description
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

{-|
Module      : Chess.Types
Description : Data types used by the 'freaky-chess' application.
Copyright   : (c) David Banas, 2023; all rights reserved World wide.
License     : BSD-3
Maintainer  : capn.freako@gmail.com
Stability   : experimental
Portability : 'stack' LTS 20.17; ASCII

The various data types needed to describe a Chess board,
as well as the play of a game.
-}
module Chess.Types
  ( Position, pattern Position, mkPosition, mkPositions, validPosition
  , Square (..), getSquare, setSquare, Board (..), isPawn, kingPos
  , Player (..), Piece (..), Direction (..), diagDirs, rectDirs, allDirs, directionSpan
  , occupied, occupiedBy, otherColor, positionsByPlayer
  , newGame, allPos, printBoard
  ) where

import qualified Data.Set    as Set
import qualified Data.Vector as V

import GHC.Generics              (Generic)
import Control.DeepSeq
import Control.Monad.State.Lazy
import Data.Char                 (chr, ord)
import Data.HashMap.Strict       (HashMap, fromList)
import Data.Maybe                (mapMaybe)
import Data.Set                  (Set)
import Data.Vector               (Vector, (//), indexed)
import System.Console.ANSI

data Board = Board
  { squares  :: Vector (Vector Square)      -- ^Array containing contents of each board square.
  , moved    :: HashMap String Bool         -- ^Records initial movement of Kings & Rooks, for castling.
  , lastMove :: Maybe Position              -- ^Destination square of last piece moved.
  , occupiedByWht :: Set (Piece, Position)  -- ^Those squares occupied by white pieces.
  , occupiedByBlk :: Set (Piece, Position)  -- ^Those squares occupied by black pieces.
  , whiteKingPos :: Position                -- ^Position of white king.
  , blackKingPos :: Position                -- ^Position of black king.
  }
  deriving (Generic, NFData)

-- |Return a new game board, set up and ready for the first move of play.
newGame :: Board
newGame = execState
  ( forM initialPlacements $
      \(pos, square) -> case square of
        Occupied clr piece -> do
          brd <- get
          let brd'@(Board _ _ _ whtSquares blkSquares _ _) = setSquare (UnsafePosition pos) square brd
              brd'' = if clr == Wht
                        then brd'{occupiedByWht = Set.insert (piece, UnsafePosition pos) whtSquares}
                        else brd'{occupiedByBlk = Set.insert (piece, UnsafePosition pos) blkSquares}
          put brd''
        _ -> error "Oops! This should never happen."
  )
  Board
    { squares = V.replicate 8 (V.replicate 8 Empty)
    , moved = fromList [ ("WK",  False)
                       , ("WKR", False)
                       , ("WQR", False)
                       , ("BK",  False)
                       , ("BKR", False)
                       , ("BQR", False)
                       ]
    , lastMove = Nothing
    , occupiedByWht = Set.empty
    , occupiedByBlk = Set.empty
    , whiteKingPos = UnsafePosition (0,4)
    , blackKingPos = UnsafePosition (7,4)
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

-- |/Smart constructor/ idiom moves validation from point of use to
-- point of creation, reducing redundant work.
--
-- __Note:__ The @UnsafePosition@ data constructor is /not/ exported.
newtype Position = UnsafePosition (Int, Int)
  deriving newtype (Eq, Ord, NFData)

instance Show Position where
  show (UnsafePosition (rank, file)) = [chr (ord 'a' + file), chr (ord '1' + rank)]

pattern Position :: Int -> Int -> Position
pattern Position rank file <- UnsafePosition (rank, file)

{-# COMPLETE Position #-}
{-# INLINE Position #-}

-- |Client code's only mechanism for making a @`Position`@.
-- Note the @Maybe@, while the pattern synonym above requires a plain @`Position`@.
-- This forces validation/filtration to the point of creation,
-- under penalty of type checking failure.
mkPosition :: (Int, Int) -> Maybe Position
mkPosition rankAndFile =
  if validPosition rankAndFile
    then Just (UnsafePosition rankAndFile)
    else Nothing

-- |Turn only the valid elements of a list of coordinates into @`Position`@s.
mkPositions :: [(Int, Int)] -> [Position]
mkPositions = mapMaybe mkPosition

validPosition :: (Int, Int) -> Bool
validPosition (r, f) = not (r < 0 || r > 7 || f < 0 || f > 7)

{-# INLINE validPosition #-}

data Square = Empty
            | Occupied Player Piece
  deriving (Eq, Generic, NFData)

instance Show Square where
  show Empty                = "   "
  show (Occupied _ piece) = " " ++ show piece ++ " "

getSquare :: Position -> Board -> Square
getSquare (Position rank file) brd = squares brd V.! rank V.! file

{-# INLINE getSquare #-}

setSquare :: Position -> Square -> Board -> Board
setSquare (Position rank file) square brd =
  brd { squares = theSquares // [(rank, (theSquares V.! rank) // [(file, square)])]
      }
 where
  theSquares = brd.squares

isPawn :: Board -> Position -> Bool
isPawn brd pos = case getSquare pos brd of
  Occupied _ P -> True
  _            -> False

-- |Return the position of a particular player's King.
kingPos :: Player -> Board -> Position
kingPos clr brd = case clr of
  Wht -> brd.whiteKingPos
  Blk -> brd.blackKingPos

data Player = Blk
            | Wht
  deriving(Eq, Generic, NFData)

data Piece = P
           | R
           | N
           | B
           | Q
           | K
  deriving (Eq, Ord, Generic, NFData)

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

data Direction = U  -- "N" is used above.
               | NE
               | E
               | SE
               | S
               | SW
               | L
               | NW

-- |Return the /diagonal/ directions, i.e., NW, SE, etc.
diagDirs :: [Direction]
diagDirs = [NW, NE, SE, SW]

-- |Return the /rectangular/ directions, i.e., E, S, etc.
rectDirs :: [Direction]
rectDirs = [U, S, E, L]

-- |Return /all/ directions.
allDirs :: [Direction]
allDirs  = diagDirs ++ rectDirs

-- |Efficient generation of radial from position to board edge.
directionSpan :: Position -> Direction -> [Position]
directionSpan (Position rank file) = \case
  U  -> [UnsafePosition (r,    file) | r <- [(rank+1)..7]]
  NE -> [UnsafePosition (r,    f)    | r <- [(rank+1)..7] | f <- [(file+1)..7]]
  E  -> [UnsafePosition (rank, f)    | f <- [(file+1)..7]]
  SE -> [UnsafePosition (r,    f)    | r <- reverse [0..(rank-1)] | f <- [(file+1)..7]]
  S  -> [UnsafePosition (r,    file) | r <- reverse [0..(rank-1)]]
  SW -> [UnsafePosition (r,    f)    | r <- reverse [0..(rank-1)] | f <- reverse [0..(file-1)]]
  L  -> [UnsafePosition (rank, f)    | f <- reverse [0..(file-1)]]
  NW -> [UnsafePosition (r,    f)    | r <- [(rank+1)..7] | f <- reverse [0..(file-1)]]
  
-- |All board positions occupied by a piece of the given color.
positionsByPlayer :: Board -> Player -> [Position]
positionsByPlayer brd = \case
  Wht -> map snd $ Set.toList brd.occupiedByWht
  Blk -> map snd $ Set.toList brd.occupiedByBlk

{-# INLINE positionsByPlayer #-}

-- |Is the given position occupied by a piece of the given color?
occupiedBy :: Board -> Position -> Player -> Bool
occupiedBy brd pos clr = case getSquare pos brd of
  Occupied clr' _ -> clr' == clr
  Empty -> False
{-# INLINE occupiedBy #-}

-- |Is the given position occupied at all?
occupied :: Board -> Position -> Bool
occupied brd pos = case getSquare pos brd of
  Occupied _ _ -> True
  Empty -> False
{-# INLINE occupied #-}

otherColor :: Player -> Player
otherColor Wht = Blk
otherColor Blk = Wht

{-# INLINE otherColor #-}

-- |Every position on the board.
allPos :: [Position]
allPos = [ UnsafePosition (rank, file)
         | rank <- [0..7]
         , file <- [0..7]
         ]

-- |The @printBoard@ and @printRank@ functions /must/ be of type @IO ()@,
-- because they rely on the @'setSGR'@ function.
printBoard :: Board -> IO ()
printBoard brd = do
  V.forM_ (V.reverse $ indexed $ squares brd) $ \rank -> do
    putStr $ show $ fst rank + 1
    putStr " "
    _ <- printRank rank
    setSGR [Reset]
    putStrLn ""
  putStrLn "   a  b  c  d  e  f  g  h"

printRank :: (Int, Vector Square) -> IO ()
printRank (rank, sqrs) =
  V.forM_ (indexed sqrs) $ \(file, square) -> do
    (if ((rank + file) `mod` 2) /= 1
      then setSGR [SetColor Background Vivid  Black]
      else setSGR [SetColor Background Dull White])
    (case square of
      Occupied Wht _ -> setSGR [SetColor Foreground Vivid White]
      _              -> setSGR [SetColor Foreground Dull  Black])
    putStr $ show square
