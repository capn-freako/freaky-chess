-- Chess move definitions.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Moves where

import Data.List  (unfoldr)
-- import Data.Maybe (catMaybes)

import Chess.Types

-- Return the list of boards corresponding to all valid moves from the
-- given square for the given player.
movesFromSquare :: Player -> Board -> Position -> [Board]
movesFromSquare color brd pos = case getSquare pos brd of
  Empty                         -> []
  Occupied clr _ | clr /= color -> []
                 | otherwise    -> [ movePiece brd pos newPos
                                   | newPos <- validNewPos brd pos
                                   ]

movePiece :: Board -> Position -> Position -> Board
movePiece brd oldPos newPos =
  let sqr = getSquare oldPos brd
   in setSquare newPos sqr $ setSquare oldPos Empty brd
  -- square <- getSquare oldPos brd
  -- setSquare oldPos Empty brd >>= setSquare newPos square

-- Return the list of valid new positions for a piece.
-- ToDo: add "en passat" P move.
validNewPos :: Board -> Position -> [Position]
validNewPos brd pos@(Position rank file) = case getSquare pos brd of
  Empty -> []
  Occupied color piece -> case piece of
    P -> case color of
      Wht -> [pos' | Just pos' <- [mkPosition (rank+1, file)],   not (occupied' pos')]
          ++ [pos' | Just pos' <- [mkPosition (rank+2, file)],   rank == 1 && not (occupied' pos')]
          ++ [pos' | Just pos' <- [mkPosition (rank+1, file-1)], occupiedBy' pos' Blk]
          ++ [pos' | Just pos' <- [mkPosition (rank+1, file+1)], occupiedBy' pos' Blk]
      Blk -> [pos' | Just pos' <- [mkPosition (rank-1, file)],   not $ occupied' pos']
          ++ [pos' | Just pos' <- [mkPosition (rank-2, file)],   rank == 6 && not (occupied' pos')]
          ++ [pos' | Just pos' <- [mkPosition (rank-1, file-1)], occupiedBy' pos' Wht]
          ++ [pos' | Just pos' <- [mkPosition (rank-1, file+1)], occupiedBy' pos' Wht]
    N -> [ pos'
         | pos' <- mkPositions [ (rank+1, file-2)
                               , (rank+2, file-1)
                               , (rank+2, file+1)
                               , (rank+1, file+2)
                               , (rank-1, file-2)
                               , (rank-2, file-1)
                               , (rank-2, file+1)
                               , (rank-1, file+2)
                               ]
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
move dir (Position rank file) = mkPosition $ case dir of
  U  -> (rank+1, file)
  NE -> (rank+1, file+1)
  E  -> (rank,   file+1)
  SE -> (rank-1, file+1)
  S  -> (rank-1, file)
  SW -> (rank-1, file-1)
  L  -> (rank,   file-1)
  NW -> (rank+1, file-1)
