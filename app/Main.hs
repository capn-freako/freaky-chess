module Main (main) where

import Chess.Play
import Chess.Types

main :: IO [()]
main = printBoard newGame
