-- Chess move definitions.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Chess.Moves
Description : Querying and execution of Chess moves.
Copyright   : (c) David Banas, 2023; all rights reserved World wide.
License     : BSD-3
Maintainer  : capn.freako@gmail.com
Stability   : experimental
Portability : 'stack' LTS 20.17; ASCII

Functions for querying the scope/validity of, as well as actually executing,
the moves on a Chess board.
-}
module Chess.Moves where

import qualified Data.Set    as Set

import Data.HashMap.Strict      (insert)
import Data.List                (unfoldr)
import Data.Maybe               (fromJust, catMaybes)

import Chess.Types

-- |Return the list of boards corresponding to all valid moves from the
-- given square for the given player.
--
-- __Note:__ Function confirms that the given player actually occupies
-- the given square, returning an empty list if not.
movesFromSquare :: Player -> Board -> Position -> [Board]
movesFromSquare color brd pos = case getSquare pos brd of
  Empty                         -> []
  Occupied clr _ | clr /= color -> []
                 | otherwise    -> catMaybes [ movePiece pos newPos brd
                                             | newPos <- validNewPos brd pos
                                             ]

-- |Updates board state according to given move, if allowed.
movePiece :: Position -> Position -> Board -> Maybe Board
movePiece oldPos@(Position rank file) newPos@(Position rank' file') brd@(Board _ _ _ whtSquares blkSquares _ _) =
  if inCheck color newBoard
    then Nothing
    else Just newBoard
 where
  (color, newBoard) = case getSquare oldPos brd of
    sqr@(Occupied clr piece) ->
      let newBoard' = setSquare newPos sqr $ setSquare oldPos Empty brd
          rsltBoard' = case clr of
            Wht -> newBoard'{ occupiedByWht = Set.insert (piece, newPos)
                                            $ Set.delete (piece, oldPos) whtSquares
                            }
            Blk -> newBoard'{ occupiedByBlk = Set.insert (piece, newPos)
                                            $ Set.delete (piece, oldPos) blkSquares
                            }
          rsltBoard''
            | rank' == 0
            = if file' == 0 then
                  rsltBoard' {moved = insert "WQR" True rsltBoard'.moved}
              else
                  if file' == 7 then
                      rsltBoard' {moved = insert "WKR" True rsltBoard'.moved}
                  else
                      rsltBoard'
            | rank' == 7
            = if file' == 0 then
                  rsltBoard' {moved = insert "BQR" True rsltBoard'.moved}
              else
                  if file' == 7 then
                      rsltBoard' {moved = insert "BKR" True rsltBoard'.moved}
                  else
                      rsltBoard'
            | otherwise = rsltBoard'
          rsltBoard = case piece of
            K -> case clr of
              Wht -> rsltBoard''{ moved = insert "WK" True rsltBoard''.moved
                               , whiteKingPos = newPos
                               }
              Blk -> rsltBoard''{ moved = insert "BK" True rsltBoard''.moved
                               , blackKingPos = newPos
                               }
            R -> case clr of
              Wht -> if rank == 0 && file == 0
                       then rsltBoard''{moved = insert "WQR" True rsltBoard''.moved}
                       else if rank == 0 && file == 7
                              then rsltBoard''{moved = insert "WKR" True rsltBoard''.moved}
                              else rsltBoard''
              Blk -> if rank == 7 && file == 0
                       then rsltBoard''{moved = insert "BQR" True rsltBoard''.moved}
                       else if rank == 7 && file == 7
                              then rsltBoard''{moved = insert "BKR" True rsltBoard''.moved}
                              else rsltBoard''
            _ -> rsltBoard''
       in case getSquare newPos brd of            -- Handle capture if necessary.
            Occupied clr' piece' -> case clr' of  -- Capture occured.
              Wht -> (clr, rsltBoard{occupiedByWht = Set.delete (piece', newPos) rsltBoard.occupiedByWht})
              Blk -> (clr, rsltBoard{occupiedByBlk = Set.delete (piece', newPos) rsltBoard.occupiedByBlk})
            _ -> (clr, rsltBoard)                 -- No capture occured.
    _ -> error $ "Oops! This should never happen."
              ++ "\nHere're the occupied squares:\n"
              ++ "Wht: " ++ show brd.occupiedByWht ++ "\n"
              ++ "Blk: " ++ show brd.occupiedByBlk ++ "\n"
              ++ "And the move requested was: " ++ show oldPos ++ " => " ++ show newPos
              ++ "\n"

-- |Return the list of valid new positions for a piece.
--
-- __ToDo:__ Add /en passat/ Pawn move.
validNewPos :: Board -> Position -> [Position]
validNewPos brd pos@(Position rank file) = case getSquare pos brd of
  Empty -> []
  Occupied color piece -> case piece of
    P -> case color of
      Wht -> [pos' | Just pos' <- [mkPosition (rank+1, file)],   not (occupied' pos')]
          ++ [pos' | Just pos' <- [ mkPosition (rank+2, file)]
                                  , rank == 1 && not (occupied' pos')
                                    && not (occupied' $ fromJust $ mkPosition (rank+1, file))
                                  ]
          ++ [pos' | Just pos' <- [mkPosition (rank+1, file-1)], occupiedBy' pos' Blk]
          ++ [pos' | Just pos' <- [mkPosition (rank+1, file+1)], occupiedBy' pos' Blk]
      Blk -> [pos' | Just pos' <- [mkPosition (rank-1, file)],   not $ occupied' pos']
          ++ [pos' | Just pos' <- [ mkPosition (rank-2, file)]
                                  , rank == 6 && not (occupied' pos')
                                    && not (occupied' $ fromJust $ mkPosition (rank-1, file))
                                  ]
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
    K -> concatMap (take 1) (reaches color allDirs)
    R -> concat             $ reaches color rectDirs
    B -> concat             $ reaches color diagDirs
    Q -> concat             $ reaches color allDirs
 where
  occupied'   = occupied   brd
  occupiedBy' = occupiedBy brd
  reaches clr = map (reach False brd pos clr)

-- |Return a piece's /reach/ in the given direction,
-- as the list of possible new positions in that direction.
--
-- __Note:__ For more efficient calculation of just the /length/ of the
-- reach, use `Chess.Play.reachLen`.
--
-- __Note:__ The first argument, when @True@, causes the function to
-- calculate /coverage/, as opposed to /movement/, the two differing by
-- one square when the final square is occupied by a player of the same
-- color.
reach :: Bool -> Board -> Position -> Player -> Direction -> [Position]
reach cover brd position color dir =
  unfoldr ( \(pos, haveCaptured) ->
              if haveCaptured
                then Nothing
                else makeMove cover brd pos color dir
          ) (position, False)

-- |Return the list of positions covered by a piece.
coveredPos :: Board -> Position -> [Position]
coveredPos brd pos@(Position rank file) = case getSquare pos brd of
  Empty                -> []
  Occupied color piece -> case piece of
    P -> case color of
      Wht -> mkPositions [(rank+1, file-1), (rank+1, file+1)]
      Blk -> mkPositions [(rank-1, file-1), (rank-1, file+1)]
    N -> mkPositions
           [ (rank+1, file-2)
           , (rank+2, file-1)
           , (rank+2, file+1)
           , (rank+1, file+2)
           , (rank-1, file-2)
           , (rank-2, file-1)
           , (rank-2, file+1)
           , (rank-1, file+2)
           ]
    K -> concatMap (take 1) $ reaches color allDirs
    R -> concat             $ reaches color rectDirs
    B -> concat             $ reaches color diagDirs
    Q -> concat             $ reaches color allDirs
 where
  reaches clr = map (reach True brd pos clr)

-- |All positions covered by the given player.
coveredBy :: Player -> Board -> [Position]
coveredBy clr brd = concatMap (coveredPos brd) (positionsByPlayer brd clr)

{-# INLINE coveredBy #-}

-- |Is a particular player's King in check?
inCheck :: Player -> Board -> Bool
inCheck clr brd = kingPos clr brd `elem` coveredBy (otherColor clr) brd

-- |Make requested move if possible and report whether a piece was captured.
--
-- If the first argument is true then check /coverage/, as opposed to /mobillity/.
makeMove :: Bool -> Board -> Position -> Player -> Direction -> Maybe (Position, (Position, Bool))
makeMove cover brd pos color dir = do
  nextPos <- move dir pos
  case getSquare nextPos brd of
    Occupied clr _ ->
      if cover || (clr == otherColor color)
        then return (nextPos, (nextPos, True))  -- We're either covering or we just captured.
        else Nothing
    _ -> return (nextPos, (nextPos, False))

-- |Calculate new position, based on current position and movement direction.
--
-- __Note:__ Checks that new square is on the board, but /not/ that it is unoccupied!
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
