-- Chess game application.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 12, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

module Main (main) where

import Control.Monad.Extra      (unfoldM)
-- import Control.Monad.State.Lazy (execStateT, get, put, lift)
import Control.Monad.Tools      ()
import Data.Char                (ord)
-- import Data.Maybe               (fromJust)
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
    return $ evalCmd brd cmd >>= \brd' -> return (rankBoard brd', brd')
  putStrLn "Finished."

evalCmd :: Board -> String -> Maybe Board
evalCmd brd cmd = do
  (from, to) <- parseCmd cmd
  if to `elem` validNewPos brd from
    then movePiece brd from to
    else Nothing

parseCmd :: String -> Maybe (Position, Position)
parseCmd cmd = case words cmd of
  []            -> Nothing
  "quit" : _    -> Nothing
  from   : wrds ->
    case wrds of
      []     -> Nothing
      to : _ -> decodeSquares (from, to)

decodeSquares :: (String, String) -> Maybe (Position, Position)
decodeSquares (wrd1, wrd2) = do
  from <- decodeSquare wrd1
  to   <- decodeSquare wrd2
  return (from, to)

decodeSquare :: String -> Maybe Position
decodeSquare str = case str of
  []              -> Nothing
  fileChar : str' ->
    case str' of
      []           -> Nothing
      rankChar : _ -> let rank = ord rankChar - ord '1'
                          file = ord fileChar - ord 'a'
                          pos  = (rank, file)
                       in if validPos pos then Just pos else Nothing
