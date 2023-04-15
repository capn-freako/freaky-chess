-- Chess game application.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 12, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad            (forM_, foldM)
import Control.Monad.Extra      (unfoldM)
import Data.Char                (ord)
import System.IO                (hFlush, stdout)

import Chess.Moves
import Chess.Play
import Chess.Types

main :: IO ()
main = do
  moves <- flip unfoldM (0, newGame) $ \(score, brd) -> do
    printBoard brd
    putStrLn $ "Score: " ++ show score
    -- Check for mate.
    case rankMoves Wht brd of
      []       -> do putStrLn "White cannot move!"
                     return Nothing
      -- ToDo: This won't detect a stalemate.
      best : _ -> let scoreChange = fst best - score
                   in if scoreChange > 9000
                        then do putStrLn "White wins!"
                                return Nothing
                        else if scoreChange < -9000
                               then do putStrLn $ "Black wins!"
                                       return Nothing
                               else do putStr "Move? "  -- Parse, validate, and execute White's move.
                                       hFlush stdout
                                       cmd <- getLine
                                       case evalCmd brd cmd of
                                         Left  msg  -> case msg of
                                                         "quit" -> return Nothing
                                                         _      -> do putStrLn msg
                                                                      return $ Just (score, (score, brd))
                                         -- If White's move is valid then calculate Black's response.
                                         Right brd' -> case rankMoves Blk brd' of
                                                         []        -> do putStrLn "Black cannot move!"
                                                                         return Nothing
                                                         best' : _ -> do let brd''  = snd best'
                                                                             score' = rankBoard brd''
                                                                         return $ Just (score', (score', brd''))
  putStrLn "Finished. Score history:"
  forM_ moves print

evalCmd :: Board -> String -> Either String Board
evalCmd brd cmd =
  foldM
    ( \brd' -> \case
        Left  msg        -> Left msg
        Right (from, to) -> do
          if to `elem` validNewPos brd' from
            then Right $ movePiece brd' from to
            else Left  $ "Requested move, " ++ show from ++ " -> " ++ show to ++ ", is invalid!"
    )
    brd
    (parseCmd cmd)

parseCmd :: String -> [Either String (Position, Position)]
parseCmd cmd = case words cmd of
  []            -> [Left "Empty command string!"]
  "quit" : _    -> [Left "quit"]
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
