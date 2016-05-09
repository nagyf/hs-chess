module Engine where

import Data.List (nub)
import Control.Monad

data PieceColor = White
                | Black
                deriving (Show, Eq)

data PieceType = Pawn
                | Knight
                | Bishop
                | Rook
                | Queen
                | King
                deriving(Show, Eq)

type Pos = (Int, Int)
data Piece = Piece PieceColor PieceType Pos deriving(Show, Eq)
type Board = [Piece]

(<+>) :: Pos -> Pos -> Pos
(x1,y1) <+> (x2,y2) = (x1+x2, y1+y2)

-- | Creates an empty board
emptyBoard :: Board
emptyBoard = []

-- | Creates an initial board with every piece in it's starting position
initialBoard :: Board
initialBoard = ws ++ bs
    where
        -- | White pieces
        ws :: [Piece]
        ws = pawns White ++ pieces White

        -- | black pieces
        bs :: [Piece]
        bs = pawns Black ++ pieces Black

        -- | Every piece except pawns
        pieces :: PieceColor -> [Piece]
        pieces White = zipWith ($) (map (Piece White) pieceRow) (row 1)
        pieces Black = zipWith ($) (map (Piece Black) pieceRow) (row 8)

        pawns :: PieceColor -> [Piece]
        pawns White = map (Piece White Pawn) (row 2)
        pawns Black = map (Piece Black Pawn)  (row 7)

        pieceRow :: [PieceType]
        pieceRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

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

-- | Returns the piece at the given position or Nothing
pieceAt :: Board -> Pos -> Maybe Piece
pieceAt b p = case length pieces of
                    0 -> Nothing
                    _ -> return $ head pieces
    where
        -- | Pieces at the given position
        pieces :: [Piece]
        pieces = filter (\(Piece _ _ pos) -> p == pos) b

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

-- | Return the positions in a row
row :: Int -> [Pos]
row n = [(x,n) | x <- [1..8]]

-- | Return the positions in a row
column :: Int -> [Pos]
column n = [(n,y) | y <- [1..8]]

-- | Every position in the given row and the given column
cross :: Pos -> [Pos]
cross (x,y) = nub $ row y ++ column x

-- | Diagonal coordinates relative to the given position
diagonals :: Pos -> [Pos]
diagonals p = filter onBoard $ nub $ map (p <+>) deltas
    where
        deltas = [(dx,dy) | dx <- [-8..8], dy <-[dx,-dx]]

-- | Returns True, if the Position is inside the board
onBoard :: Pos -> Bool
onBoard (x, y) = x > 0 && y > 0 && x < 9 && y < 9
