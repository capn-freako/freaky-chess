-- Chess move definitions.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 9, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Chess.Moves where

-- Return the list of valid moves from the given square for the given player.
movesFromSquare :: Color -> Board -> Position -> [Board]
movesFromSquare color brd pos = case (getSquare pos brd) of
  Nothing                                  -> []
  Just Empty                               -> []
  Just (Occupied clr piece) | clr /= color -> []
                            | otherwise    -> catMaybes [ movePiece brd pos newPos
                                                        | newPos <- validNewPos brd pos
                                                        ]

getSquare :: Position -> Board -> Maybe Square
square pos brd
  | posValid pos = Just (brd ! rank ! file)
  | otherwise    = Nothing

setSquare :: Position -> Square -> Board -> Maybe Board
setSquare pos square brd
  | posValid (rank, file) = Just (replace rank (replace file square (brd !! rank)) brd)
  | otherwise             = Nothing

replace :: Int -> a -> List a -> List a
replace ix x xs = take ix xs ++ x : drop (ix + 1) xs

posValid :: Position -> Bool
posValid (rank, file) = !(rank < 0 || rank > 7 || file < 0 || file > 7)

movePiece :: Board -> Position -> Position -> Maybe Board
movePiece brd oldPos newPos =
  square <- getSquare oldPos brd
  setSquare oldPos Empty brd >>= setSquare newPos square

-- ToDo: add "en passat" pawn move.
validNewPos :: Board -> Position -> List Position
validNewPos brd pos@(rank, file) = case getSquare pos brd of
  Nothing     -> []
  Just square -> case square of
    Empty                -> []
    Occupied color piece -> case piece of
      Pawn   -> case color of
        White -> occupied (rank+1, file) ? [] : [(rank+1, file)]
              ++ rank == 1 && !occupied (rank+2, file) ? [(rank+2, file)] : []
              ++ occupiedBy Black (rank+1, file-1) ? [(rank+1, file-1)] : []
              ++ occupiedBy Black (rank+1, file+1) ? [(rank+1, file+1)] : []
        Black -> occupied (rank-1, file) ? [] : [(rank-1, file)]
              ++ rank == 6 && ! occupied (rank-2, file) ? [(rank-2, file)] : []
              ++ occupiedBy White (rank-1, file-1) ? [(rank-1, file-1)] : []
              ++ occupiedBy White (rank-1, file+1) ? [(rank-1, file+1)] : []
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
                , ! occupiedBy color pos'
                ]
      King   -> map fst $ catMaybes $ [ makeMove dir color pos
                                      | dir <- [Up, Down, Left, Right, UpLeft, DownLeft, DownLeft, UpRight]
                                      ]
      Rook   -> concatenate [span dir color pos | dir <- [Up, Down, Left, Right]]
      Bishop -> concatenate [span dir color pos | dir <- [UpLeft, DownLeft, DownLeft, UpRight]]
      Queen  -> concatenate [span dir color pos | dir <- [Up, Down, Left, Right, UpLeft, DownLeft, DownLeft, UpRight]]

-- Return available span in the given direction.
span :: Direction -> Color -> Position -> List Position
span dir color position = map fst $ catMaybes $
  unfoldr ( \(pos, haveCaptured) ->
              if haveCaptured
                then Nothing
                else makeMove dir color pos
          ) (position, False)

-- Make requested move if possible and report whether a piece was captured.
makeMove :: Direction -> Color -> Position -> Maybe (Position, Bool)
makeMove dir color pos = do
  nextPos <- move dir pos
  if occupiedBy color nextPos  -- Bumped into one of our own pieces.
    then Nothing
    else if occupiedBy (otherColor color) nextPos
           then Just (nextPos, True)
           else Just (nextPos, False)

-- Calculate new position, based on current position and movement direction.
--
-- Checks that new square is on the board, but not that it is unoccupied!
move :: Direction -> Position -> Maybe Position
move dir (rank, file) =
  let newPos = case dir of
    Up        -> (rank+1, file)
    UpRight   -> (rank+1, file+1)
    Right     -> (rank,   file+1)
    DownRight -> (rank-1, file+1)
    Down      -> (rank-1, file)
    DownLeft  -> (rank-1, file-1)
    Left      -> (rank,   file-1)
    UpLeft    -> (rank+1, file-1)
   in if validPos newPos then newPos
                         else Nothing
