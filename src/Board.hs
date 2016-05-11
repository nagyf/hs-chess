module Board where

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

-- | The position of a piece where the first coordinate represents the column
--   and the second represents the row
type Pos = (Int, Int)

-- | Represents a piece on the board
data Piece = Piece PieceColor PieceType Pos deriving(Show, Eq)

-- | Represents a chess board
type Board = [Piece]

-- | Adds up 2 position vectors by coordinates
infixr 9 <+>
(<+>) :: Pos -> Pos -> Maybe Pos
(x1, y1) <+> (x2, y2) =
    let pos' = (x1+x2, y1+y2)
    in if onBoard pos' then Just pos' else Nothing

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
        pawns Black = map (Piece Black Pawn)  (row 7)

        pieceRow :: [PieceType]
        pieceRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- | Returns the piece at the given position or Nothing
pieceAt :: Board -> Pos -> Maybe Piece
pieceAt board p = case length pieces of
                    0 -> Nothing
                    _ -> return $ head pieces
    where
        -- | Pieces at the given position
        pieces :: [Piece]
        pieces = filter (\(Piece _ _ pos) -> p == pos) board

-- | Return the positions in a row
row :: Int -> [Pos]
row n = [(x,n) | x <- [1..8]]

-- | Return the positions in a row
column :: Int -> [Pos]
column n = [(n,y) | y <- [1..8]]

-- | Returns True, if the Position is inside the board
onBoard :: Pos -> Bool
onBoard (x, y) = x > 0 && y > 0 && x < 9 && y < 9