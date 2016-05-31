module Engine where

import Control.Monad
import Board
import Geometry
import Data.List (nub)

-- | Return the white pieces from the board
whites :: Board -> [Piece]
whites = filter (\piece -> color piece == White)

-- | Return the black pieces from the board
blacks :: Board -> [Piece]
blacks = filter (\piece -> color piece == Black)

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
moves (Piece _ Queen p) = allDirections p
moves (Piece _ King p) = ambientPos p
moves (Piece _ Knight (x,y)) = do
    let ds = [-2, -1, 1, 2]
    dx <- ds
    dy <- ds
    guard $ abs dx /= abs dy && onBoard (x+dx,y+dy)
    return (x+dx, y+dy)

-- | Return the legal moves of the piece
legalMoves :: Board -> Piece -> [Pos]
legalMoves b piece@(Piece c King _)   = filter (emptyOrEnemy b c) $ moves piece
legalMoves b piece@(Piece c Knight _) = filter (emptyOrEnemy b c) $ moves piece
legalMoves b piece@(Piece c Pawn _)   = filter (emptyOrEnemy b c) $ moves piece
legalMoves b piece@(Piece c Bishop p) = nub $ legalMovesInLine b p (moves piece)
legalMoves b piece@(Piece c Rook _)   = filter (emptyOrEnemy b c) $ moves piece
legalMoves b piece@(Piece c Queen _)  = filter (emptyOrEnemy b c) $ moves piece

legalMovesInLine :: Board -> Pos -> [Pos] -> [Pos]
legalMovesInLine _ _ [] = []
legalMovesInLine b start (p:ps) =
    if emptySegment b (segment start p)
        then p : legalMovesInLine b start ps
        else legalMovesInLine b start ps

-- | Check if a position is empty or contains an enemy piece
emptyOrEnemy :: Board -> PieceColor -> Pos -> Bool
emptyOrEnemy b c p = enemy b c p || empty b p

-- | Check if a position is contains an enemy piece
enemy :: Board -> PieceColor -> Pos -> Bool
enemy b c p = case pieceAt b p of
    Just piece  -> color piece == enemyColor c
    Nothing     -> False

-- | Check if a position is empty
empty :: Board -> Pos -> Bool
empty b p = case pieceAt b p of
    Just _  -> False
    Nothing -> True

-- | Check if every position of a line is empty
emptySegment :: Board -> Maybe Segment -> Bool
emptySegment _ Nothing = False
emptySegment b (Just s) = all (empty b) (points s)