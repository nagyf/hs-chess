module App where

import Pretty
import Board
import SAN

-- | The main method of the application, that runs a test game
main :: IO ()
main = do
    let inputFile = "../test/test.dat"
    moves <- loadMoves inputFile
    case moves of
        Right ms -> runGame ms
        Left msg -> putStrLn $ "Error in input file: " ++ msg

-- | Run the game represented by a series of moves, and print the result
runGame :: [Move] -> IO ()
runGame ms = do
    let result = applyMoves initialBoard ms
    putStrLn $ prettyBoard result

-- | Load a series of moves from a text file using the SAN module
loadMoves :: FilePath -> IO (Either String [Move])
loadMoves path = do
    content <- readFile path
    return $ parseMoves content

-- | Apply the moves on the board and return the resulting board
applyMoves :: Board -> [Move] -> Board
applyMoves = foldl applyMove

-- | Apply a single move on the board, and return the resulting board
applyMove :: Board -> Move -> Board
applyMove = undefined