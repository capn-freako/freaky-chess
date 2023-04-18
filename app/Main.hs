-- Chess game application.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 12, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Control.Monad.Extra      (unfoldM)
import Control.Monad.State.Lazy
import Data.Char                (ord)
-- import Data.Foldable            (foldl')
import System.IO                (hFlush, stdout)

import Chess.Moves
import Chess.Play
import Chess.Types

main :: IO ()
main = do
  scores <- unfoldM iter [(0, newGame)]
  putStrLn "Finished. Score history:"
  forM_ scores print

iter :: [(Int, Board)] -> IO (Maybe (Int, [(Int, Board)]))
iter previousMoves = do
  let (score, brd) = last previousMoves
  printBoard brd
  putStrLn $ "Score: " ++ show score
  -- Check for mate.
  let (score', _) = bestMove 1 Wht brd
      scoreChange = score' - score
   in if scoreChange > 9000  -- ToDo: This won't detect a stalemate.
        then do putStrLn "White wins!"
                return Nothing
        else if scoreChange < -9000
               then do putStrLn $ "Black wins!"
                       return Nothing
               else do putStr "Move? "  -- Parse, validate, and execute White's move.
                       hFlush stdout
                       cmd <- getLine
                       case evalCmd brd cmd of
                         Left  msg  -> do
                           case msg of
                             "quit" -> return Nothing
                             "back" -> let oldPrevMoves = init previousMoves
                                           (score'', _) = last $ oldPrevMoves
                                        in return $ Just (score'', oldPrevMoves)
                             _      -> do putStrLn msg
                                          return $ Just (score, previousMoves)
                         -- If White's move is valid then calculate Black's response.
                         Right brd' -> do
                           putStrLn "Thinking..."
                           let (_, brd'') = bestMove 3 Blk brd'
                               score''    = rankBoard brd''
                            in return $ Just (score'', previousMoves ++ [(score'', brd'')])

evalCmd :: Board -> String -> Either String Board
evalCmd brd cmd = case parsedCmds of
  [] -> Left "Empty list from parseCmd!"
  -- mv : _ -> mv >>= \(from, to) ->
  mv : _ -> case mv of
    Right (from, to) ->
      if from `elem` brd.occupiedByWht
        then let possibleTos = validNewPos brd from
              in if to `elem` possibleTos
                   then Right $ movePiece brd from to
                   else Left  $ "Requested move, " ++ show from ++ " -> " ++ show to ++ ", is invalid!\n"
                                ++ "  Valid destination squares are: " ++ show possibleTos ++ "\n"
                                ++ "  Received command: " ++ cmd ++ "\n"
                                ++ "  Parsed command: " ++ show parsedCmds
        else Left $ "Square " ++ show from ++ " does not contain a white piece!"
    Left err -> Left err
  -- foldM
  --   ( \brd' -> \case
  --       Left  msg        -> Left msg
  --       Right (from, to) ->
  --         if from `elem` brd'.occupiedByWht
  --           then let possibleTos = validNewPos brd' from
  --                 in if to `elem` possibleTos
  --                      then Right $ movePiece brd' from to
  --                      else Left  $ "Requested move, " ++ show from ++ " -> " ++ show to ++ ", is invalid!\n"
  --                                   ++ "  Valid destination squares are: " ++ show possibleTos ++ "\n"
  --                                   ++ "  Received command: " ++ cmd ++ "\n"
  --                                   ++ "  Parsed command: " ++ show parsedCmds
  --           else Left $ "Square " ++ show from ++ " does not contain a white piece!"
  --   )
  --   brd
  --   parsedCmds
 where
  parsedCmds = parseCmd cmd

-- List supports castling, which requires two moves.
parseCmd :: String -> [Either String (Position, Position)]
parseCmd cmd = case words cmd of
  []            -> [Left "Empty command string!"]
  "quit" : _    -> [Left "quit"]
  "back" : _    -> [Left "back"]
  "o-o"  : _    -> [decodeSquares ("h1", "f1"), decodeSquares ("e1", "g1")]
  from   : wrds ->
    case wrds of
      []     -> [Left "Missing destination square!"]
      to : _ -> [decodeSquares (from, to)]

decodeSquares :: (String, String) -> Either String (Position, Position)
decodeSquares (wrd1, wrd2) = do
  from <- decodeSquare wrd1
  to   <- decodeSquare wrd2
  return (from, to)

decodeSquare :: String -> Either String Position
decodeSquare str = case str of
  []              -> Left "Empty string!"
  fileChar : str' ->
    case str' of
      []           -> Left "Missing rank character!"
      rankChar : _ -> let rank = ord rankChar - ord '1'
                          file = ord fileChar - ord 'a'
                          pos  = (rank, file)
                       in case mkPosition pos of
                            Nothing   -> Left $ "Invalid position: " ++ fileChar : [rankChar]
                            Just pos' -> Right pos'
