module Engine where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Control.Monad
import Board

-- | Return the white pieces from the board
whites :: Board -> [Piece]
whites = filter (\(Piece c _ _) -> c == White)

-- | Return the black pieces from the board
blacks :: Board -> [Piece]
blacks = filter (\(Piece c _ _) -> c == Black)

-- | Return the value of the piece
valuePiece :: Piece -> Int
valuePiece (Piece _ Pawn _) = 1
valuePiece (Piece _ Knight _) = 3
valuePiece (Piece _ Bishop _) = 3
valuePiece (Piece _ Rook _) = 5
valuePiece (Piece _ Queen _) = 9
valuePiece (Piece _ King _) = 1000

-- | Return the possible moves for any piece
moves :: Piece -> [Pos]
moves (Piece White Pawn (x,y)) = [(x,y+1)]
moves (Piece Black Pawn (x,y)) = [(x,y-1)]
moves (Piece _ Bishop p) = diagonals p
moves (Piece _ Rook p) = cross p
moves (Piece _ Queen p) = nub $ cross p ++ diagonals p
moves (Piece _ King p) = ambientPos p
moves (Piece _ Knight (x,y)) = do
    let ds = [-2, -1, 1, 2]
    dx <- ds
    dy <- ds
    guard $ abs dx /= abs dy && onBoard (x+dx,y+dy)
    return (x+dx,y+dy)

-- | Return the ambient of a number
ambient :: Int -> [Int]
ambient x = [x-1, x, x+1]

-- | Return the positions around a given position
ambientPos :: Pos -> [Pos]
ambientPos (x,y) =
    [(x',y') | x' <- ambient x, y' <- ambient y, onBoard (x',y')]

-- | Every position in the given row and the given column
cross :: Pos -> [Pos]
cross (x,y) = nub $ row y ++ column x

-- | Diagonal coordinates relative to the given position
diagonals :: Pos -> [Pos]
diagonals p = nub $ mapMaybe (p <+>) deltas
    where
        deltas = [(dx,dy) | dx <- [-8..8], dy <-[dx,-dx]]

