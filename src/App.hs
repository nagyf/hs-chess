module App where

import Pretty
import Board

main :: IO ()
main = putStrLn $ prettyBoard initialBoard