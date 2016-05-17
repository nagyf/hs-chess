module Pretty (prettyBoard) where

import Board

-- | Pretty print the board
prettyBoard :: Board -> String
prettyBoard = concat . prettyRows 8

prettyRows :: Int -> Board -> [String]
prettyRows 0 _ = []
prettyRows n b = prettyRow n b : "\n": prettyRows (n - 1) b
    where
        prettyRow :: Int -> Board -> String
        prettyRow m bb =
            let foundPieces = map (pieceAt bb) [(x, m) | x <- [1..8]]
                rowHeader = show m ++ " |"
            in rowHeader ++ concatMap (fromMaybe' " .. " prettyPiece) foundPieces

-- | Pretty print a piece
prettyPiece :: Piece -> String
prettyPiece p = wrap " " $ prettyColor (color p) ++ showPiece p
    where
        showPiece :: Piece -> String
        showPiece (Piece _ Pawn _ ) = "P"
        showPiece (Piece _ Bishop _ ) = "B"
        showPiece (Piece _ Knight _ ) = "N"
        showPiece (Piece _ Rook _ ) = "R"
        showPiece (Piece _ King _ ) = "K"
        showPiece (Piece _ Queen _ ) = "Q"

-- | Pretty print a color
prettyColor :: PieceColor -> String
prettyColor White = "W"
prettyColor Black = "B"

-- | Given a default value and a function, if the value is nothing return the
-- default value, else execute the function on the Just value and return that
fromMaybe' :: a -> (b -> a) -> Maybe b -> a
fromMaybe' def _ Nothing = def
fromMaybe' _ f (Just x) = f x

wrap :: String -> String -> String
wrap border str = border ++ str ++ border
