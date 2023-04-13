-- Chess move definitions.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Moves where

import Data.List  (unfoldr)
import Data.Maybe (catMaybes)

import Chess.Types

-- Return the list of boards corresponding to all valid moves from the
-- given square for the given player.
movesFromSquare :: Player -> Board -> Position -> [Board]
movesFromSquare color brd pos = case getSquare pos brd of
  Nothing                                  -> []
  Just Empty                               -> []
  Just (Occupied clr _) | clr /= color -> []
                        | otherwise    -> catMaybes [ movePiece brd pos newPos
                                                    | newPos <- validNewPos brd pos
                                                    ]

movePiece :: Board -> Position -> Position -> Maybe Board
movePiece brd oldPos newPos = do
  square <- getSquare oldPos brd
  setSquare oldPos Empty brd >>= setSquare newPos square

-- Return the list of valid new positions for a piece.
-- ToDo: add "en passat" P move.
validNewPos :: Board -> Position -> [Position]
validNewPos brd pos@(rank, file) = case getSquare pos brd of
  Nothing     -> []
  Just square -> case square of
    Empty                -> []
    Occupied color piece -> case piece of
      P -> case color of
        Wht -> [(rank+1, file)   | not $ occupied' (rank+1, file)]
            ++ [(rank+2, file)   | rank == 1 && not (occupied' (rank+2, file))]
            ++ [(rank+1, file-1) | occupiedBy' (rank+1, file-1) Blk]
            ++ [(rank+1, file+1) | occupiedBy' (rank+1, file+1) Blk]
        Blk -> [(rank-1, file)   | not $ occupied' (rank-1, file)]
            ++ [(rank-2, file)   | rank == 6 && not (occupied' (rank-2, file))]
            ++ [(rank-1, file-1) | occupiedBy' (rank-1, file-1) Wht]
            ++ [(rank-1, file+1) | occupiedBy' (rank-1, file+1) Wht]
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
           , not $ occupiedBy' pos' color
           ]
      K -> concatMap (take 1) $ reaches color allDirs
      R -> concat             $ reaches color rectDirs
      B -> concat             $ reaches color diagDirs
      Q -> concat             $ reaches color allDirs
 where
  occupied'   = occupied   brd
  occupiedBy' = occupiedBy brd
  reaches clr = map (reach False brd pos clr)

-- Return available reach in the given direction.
reach :: Bool -> Board -> Position -> Player -> Direction -> [Position]
reach cover brd position color dir =
  unfoldr ( \(pos, haveCaptured) ->
              if haveCaptured
                then Nothing
                else makeMove cover brd pos color dir
          ) (position, False)

-- Make requested move if possible and report whether a piece was captured.
makeMove :: Bool -> Board -> Position -> Player -> Direction -> Maybe (Position, (Position, Bool))
makeMove cover brd pos color dir = do
  nextPos <- move dir pos
  if occupiedBy brd nextPos color  -- Bumped into one of our own pieces.
    then if cover then Just (nextPos, (nextPos, True))  -- Covering, so count it.
                  else Nothing                          -- Moving, so don't.
    else if occupiedBy brd nextPos (otherColor color)
           then Just (nextPos, (nextPos, True))
           else Just (nextPos, (nextPos, False))

-- Calculate new position, based on current position and movement direction.
--
-- Checks that new square is on the board, but not that it is unoccupied!
move :: Direction -> Position -> Maybe Position
move dir (rank, file) =
  let newPos = case dir of
        U  -> (rank+1, file)
        NE -> (rank+1, file+1)
        E  -> (rank,   file+1)
        SE -> (rank-1, file+1)
        S  -> (rank-1, file)
        SW -> (rank-1, file-1)
        L  -> (rank,   file-1)
        NW -> (rank+1, file-1)
   in if validPos newPos then Just newPos
                         else Nothing
