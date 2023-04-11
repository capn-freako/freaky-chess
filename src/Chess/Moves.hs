-- Chess move definitions.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Moves where

import Data.List  (unfoldr)
-- import Data.Maybe (mapMaybe, catMaybes)
import Data.Maybe (catMaybes)

import Chess.Types

-- Return the list of valid moves from the given square for the given player.
movesFromSquare :: Color -> Board -> Position -> [Board]
movesFromSquare color brd pos = case getSquare pos brd of
  Nothing                                  -> []
  Just Empty                               -> []
  Just (Occupied clr _) | clr /= color -> []
                        | otherwise    -> catMaybes [ movePiece brd pos newPos
                                                    | newPos <- validNewPos brd pos
                                                    ]

getSquare :: Position -> Board -> Maybe Square
getSquare pos@(rank, file) brd
  | posValid pos = Just (brd !! rank !! file)
  | otherwise    = Nothing

setSquare :: Position -> Square -> Board -> Maybe Board
setSquare pos@(rank, file) square brd
  | posValid pos = Just (replace rank (replace file square (brd !! rank)) brd)
  | otherwise    = Nothing

replace :: Int -> a -> [a] -> [a]
replace ix x xs = take ix xs ++ x : drop (ix + 1) xs

posValid :: Position -> Bool
posValid (rank, file) = not (rank < 0 || rank > 7 || file < 0 || file > 7)

movePiece :: Board -> Position -> Position -> Maybe Board
movePiece brd oldPos newPos = do
  square <- getSquare oldPos brd
  setSquare oldPos Empty brd >>= setSquare newPos square

-- ToDo: add "en passat" pawn move.
validNewPos :: Board -> Position -> [Position]
validNewPos brd pos@(rank, file) = case getSquare pos brd of
  Nothing     -> []
  Just square -> case square of
    Empty                -> []
    Occupied color piece -> case piece of
      Pawn   -> case color of
        White -> [(rank+1, file)   | not $ occupied' (rank+1, file)]
              ++ [(rank+2, file)   | rank == 1 && not (occupied' (rank+2, file))]
              ++ [(rank+1, file-1) | occupiedBy' (rank+1, file-1) Black]
              ++ [(rank+1, file+1) | occupiedBy' (rank+1, file+1) Black]
        Black -> [(rank-1, file)   | not $ occupied' (rank-1, file)]
              ++ [(rank-2, file)   | rank == 6 && not (occupied' (rank-2, file))]
              ++ [(rank-1, file-1) | occupiedBy' (rank-1, file-1) White]
              ++ [(rank-1, file+1) | occupiedBy' (rank-1, file+1) White]
      Knight -> [ pos'
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
      King   -> concatMap (take 1) $ reaches color allDirs
      Rook   -> concat             $ reaches color rectDirs
      Bishop -> concat             $ reaches color diagDirs
      Queen  -> concat             $ reaches color allDirs
 where
  occupied'   = occupied   brd
  occupiedBy' = occupiedBy brd
  reaches clr = map (reach brd pos clr)

-- Return available reach in the given direction.
reach :: Board -> Position -> Color -> Direction -> [Position]
reach brd position color dir =
  unfoldr ( \(pos, haveCaptured) ->
              if haveCaptured
                then Nothing
                else makeMove brd pos color dir
          ) (position, False)

-- Make requested move if possible and report whether a piece was captured.
makeMove :: Board -> Position -> Color -> Direction -> Maybe (Position, (Position, Bool))
makeMove brd pos color dir = do
  nextPos <- move dir pos
  if occupiedBy brd nextPos color  -- Bumped into one of our own pieces.
    then Nothing
    else if occupiedBy brd nextPos (otherColor color)
           then Just (nextPos, (nextPos, True))
           else Just (nextPos, (nextPos, False))

-- Calculate new position, based on current position and movement direction.
--
-- Checks that new square is on the board, but not that it is unoccupied!
move :: Direction -> Position -> Maybe Position
move dir (rank, file) =
  let newPos = case dir of
        N  -> (rank+1, file)
        NE -> (rank+1, file+1)
        E  -> (rank,   file+1)
        SE -> (rank-1, file+1)
        S  -> (rank-1, file)
        SW -> (rank-1, file-1)
        W  -> (rank,   file-1)
        NW -> (rank+1, file-1)
   in if validPos newPos then Just newPos
                         else Nothing
