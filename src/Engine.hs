module Engine where

import Data.List (nub)
import Data.Maybe (mapMaybe)
import Control.Monad
import Board

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
moves (Piece _ Queen p) = nub $ cross p ++ diagonals p
moves (Piece _ King p) = ambientPos p
moves (Piece _ Knight (x,y)) = do
    let ds = [-2, -1, 1, 2]
    dx <- ds
    dy <- ds
    guard $ abs dx /= abs dy && onBoard (x+dx,y+dy)
    return (x+dx,y+dy)

-- | Return the legal moves of the piece
legalMoves :: Board -> Piece -> [Pos]
legalMoves = undefined

-- | Check if a position is empty or contains an enemy piece
emptyOrEnemy :: Board -> PieceColor -> Pos -> Bool
emptyOrEnemy b c p = case (pieceAt b p) of
    Just piece  -> color piece == c
    Nothing     -> True

emptyLine :: Board -> Pos -> Pos -> Bool
emptyLine b start end = undefined

-- | Return every position in a line between 2 positions
segment :: Pos -> Pos -> [Pos]
segment p1 p2 =
    let m         = slope p1 p2
        (pp1,pp2) = order p1 p2
    in  case abs m of
            1.0 -> diagonalPoints (round m) pp1 pp2
            0.0 -> normalPoints pp1 pp2
            _   -> []

    where
        diagonalPoints m p1@(sx, sy) p2
            | p1 == p2 = [p1]
            | otherwise = (sx, sy) : diagonalPoints m (sx + abs m, sy + m) p2

        normalPoints (sx,sy) p2@(ex,ey)
            | sx == ex && sy == ey = [(sx,sy)]
            | sx /= ex  = (sx,sy) : normalPoints (sx + 1, sy) p2
            | otherwise = (sx,sy) : normalPoints (sx, sy + 1) p2

        order p1 p2
            | p1 < p2 = (p1,p2)
            | otherwise = (p2,p1)

-- | Return the slope of the line between 2 points
slope :: Pos -> Pos -> Float
slope (x1,y1) (x2,y2) =
    let (xx1,yy1) = (fromIntegral x1, fromIntegral y1)
        (xx2,yy2) = (fromIntegral x2, fromIntegral y2)
    in  if xx2 - xx1 == 0 then 0 else (yy2 - yy1) / (xx2 - xx1)

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

