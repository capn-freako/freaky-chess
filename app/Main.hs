-- Chess game application.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original data:   April 12, 2023
--
-- Copyright (c) 2023 David Banas; all rights reserved World wide.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Foldl            (mean, fold)
import Control.Monad.Extra      (unfoldM)
import Control.Monad.State.Lazy
import Data.Char                (ord)
import System.IO                (hFlush, stdout)
import System.TimeIt

import Chess.Moves
import Chess.Play
import Chess.Types

main :: IO ()
main = do
  (scores, perfs) <- unzip <$> unfoldM iter [(0, (newGame, newGame), 0)]
  putStrLn "Finished."
  putStrLn $ "Average performance: " ++ show (round $ fold mean $ map fromIntegral perfs) ++ " Moves/s"
  putStrLn "Score history:"
  forM_ scores print

iter :: [(Int, (Board, Board), Int)] -> IO (Maybe ((Int, Int), [(Int, (Board, Board), Int)]))
iter previousMoves = do
  let (score, (boardAfterLastWhiteMove, brd), perf) = last previousMoves
  printBoard brd
  putStrLn $ "Score: " ++ show score
  -- Check for (stale)mate.
  case allMoves Wht brd of
    [] -> if inCheck Wht brd
            then do putStrLn "Black wins!"
                    return Nothing
            else do putStrLn "Stalemate."
                    return Nothing
    _ -> do putStr "Move? "  -- Parse, validate, and execute White's move.
            hFlush stdout
            cmd <- getLine
            case evalCmd brd cmd of
              Left  msg  -> do
                case msg of
                  "quit" -> return Nothing
                  "back" -> let oldPrevMoves = init previousMoves
                                (score'', _, perf'') = last $ oldPrevMoves
                             in return $ Just ((score'', perf''), oldPrevMoves)
                  "trace" -> do putStrLn "*** Tracing AI's projected board evolution, starting from White's last move:"
                                printBoard boardAfterLastWhiteMove
                                doTrace 4 Blk boardAfterLastWhiteMove
                                putStrLn "*** End of trace."
                                return $ Just ((score, perf), previousMoves)
                  _      -> do putStrLn msg
                               return $ Just ((score, perf), previousMoves)
              Right brd' -> do  -- If White's move is valid then...
                -- check for (stale)mate,
                case allMoves Blk brd' of
                  [] -> if inCheck Blk brd'
                           then do putStrLn "White wins!"
                                   printBoard brd'
                                   return Nothing
                           else do putStrLn "Stalemate."
                                   return Nothing
                  _ -> do putStr "Thinking..."  -- and calculate Black's response.
                          hFlush stdout
                          (nSecs, (nMoves, score'', brd'')) <- timeItT $ do
                            let ((!futureScore, !brd''), !nMoves) = bestMove 3 Blk brd'
                                !score''                          = rankBoard brd''
                            return (nMoves, score'', brd'')
                          let perf'' = round $ fromIntegral nMoves / nSecs
                          putStrLn $ "Done. " ++ show nMoves ++ " moves tried in "
                            ++ show nSecs ++ " seconds (" ++ show perf'' ++ " moves/s)."
                          return $ Just ((score'', perf''), previousMoves ++ [(score'', (brd', brd''), perf'')])

evalCmd :: Board -> String -> Either String Board
evalCmd brd cmd =
  foldM
    ( \brd' -> \case
        Left  msg -> Left msg
        Right ((from, to), specialMove) ->
          if from `elem` brd'.occupiedByWht
            then let possibleTos = validNewPos brd' from
                  in if to `elem` possibleTos || specialMove
                       then case movePiece from to brd' of
                         Nothing    -> Left "Sorry, that move is invalid."
                         Just brd'' -> Right brd''
                       else Left  $ "Requested move, " ++ show from ++ " -> " ++ show to ++ ", is invalid!\n"
                                    ++ "  Valid destination squares are: " ++ show possibleTos ++ "\n"
                                    ++ "  Received command: " ++ cmd ++ "\n"
                                    ++ "  Parsed command: " ++ show parsedCmds ++ "\n"
                                    ++ "  Occupied by White: " ++ show brd'.occupiedByWht ++ "\n"
                                    ++ "  Occupied by Black: " ++ show brd'.occupiedByBlk ++ "\n"
            else Left $ "Square " ++ show from ++ " does not contain a white piece!"
    )
    brd
    parsedCmds
 where
  parsedCmds = parseCmd cmd

-- List supports castling, which requires two moves.
parseCmd :: String -> [Either String ((Position, Position), Bool)]
parseCmd cmd = case words cmd of
  []            -> [Left "Empty command string!"]
  "quit" : _    -> [Left "quit"]
  "back" : _    -> [Left "back"]
  "trace" : _   -> [Left "trace"]
  "o-o"  : _    -> map (fmap (, True)) [decodeSquares ("h1", "f1"), decodeSquares ("e1", "g1")]
  "o-o-o" : _   -> map (fmap (, True)) [decodeSquares ("a1", "d1"), decodeSquares ("e1", "c1")]
  from   : wrds ->
    case wrds of
      []     -> [Left "Missing destination square!"]
      to : _ -> map (fmap (, False)) [decodeSquares (from, to)]

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

-- Trace/debug last AI move selection.
doTrace :: Int -> Player -> Board -> IO ()
doTrace n clr brd = do
  let ((score, brd'), _) = bestMove n clr brd
  printBoard brd'
  putStrLn $ "AI's predicted future score: " ++ show score
  case n of
    0 -> return ()
    _ -> doTrace (n-1) (otherColor clr) brd'
