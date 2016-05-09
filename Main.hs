module Main where

import Engine
import Pretty

main :: IO ()
main = putStrLn $ prettyBoard initialBoard
