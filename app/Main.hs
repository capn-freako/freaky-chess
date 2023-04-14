-- Chess game application.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 12, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Main (main) where

import Control.Monad            (forM_)
import Control.Monad.Extra      (unfoldM)
import Control.Monad.Tools      ()
import Data.Char                (ord)
import System.IO                (hFlush, stdout)

import Chess.Moves
import Chess.Play
import Chess.Types

main :: IO ()
main = do
  moves <- flip unfoldM newGame $ \brd -> do
    printBoard brd
    putStr "Move? "
    hFlush stdout
    cmd <- getLine
    case evalCmd brd cmd of
      Left  msg  -> case msg of
                      "quit" -> return Nothing
                      _      -> do putStrLn msg
                                   return $ Just (rankBoard brd, brd)
      Right brd' -> case rankMoves Blk brd' of
                      []       -> do putStrLn "Black cannot move!"
                                     return Nothing
                      best : _ -> return $ Just best
  putStrLn "Finished. Score history:"
  forM_ moves print

evalCmd :: Board -> String -> Either String Board
evalCmd brd cmd = do
  (from, to) <- parseCmd cmd
  if to `elem` validNewPos brd from
    then Right $ movePiece brd from to
    else Left "Requested move is invalid!"

parseCmd :: String -> Either String (Position, Position)
parseCmd cmd = case words cmd of
  []            -> Left "Empty command string!"
  "quit" : _    -> Left "quit"
  from   : wrds ->
    case wrds of
      []     -> Left "Missing destination square!"
      to : _ -> decodeSquares (from, to)

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
