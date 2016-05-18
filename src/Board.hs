module Board where

import Data.Maybe (listToMaybe)
import Geometry

-- | The color of a piece
data PieceColor = White
                | Black
                deriving (Show, Eq)

-- | The type of a piece
data PieceType = Pawn
                | Knight
                | Bishop
                | Rook
                | Queen
                | King
                deriving(Show, Eq)

-- | Represents a piece on the board
data Piece = Piece { color       :: PieceColor,
                     pieceType   :: PieceType,
                     pos         :: Pos
                    } deriving (Show, Eq)

-- | Represents a chess board
type Board = [Piece]

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

        -- | Returns the Pawns for the given color
        pawns :: PieceColor -> [Piece]
        pawns White = map (Piece White Pawn) (row 2)
        pawns Black = map (Piece Black Pawn) (row 7)

        pieceRow :: [PieceType]
        pieceRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- | Returns the piece at the given position or Nothing
pieceAt :: Board -> Pos -> Maybe Piece
pieceAt board p = listToMaybe pieces
    where
        -- | Pieces at the given position
        pieces :: [Piece]
        pieces = filter (\piece -> p == pos piece) board

enemyColor :: PieceColor -> PieceColor
enemyColor White = Black
enemyColor Black = White