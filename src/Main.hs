module Main where

import Engine
import Pretty
import Board

main :: IO ()
main = putStrLn $ prettyBoard initialBoard
