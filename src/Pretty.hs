module Pretty (prettyBoard) where

import Engine

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
            in rowHeader ++ concatMap prettyPiece foundPieces

prettyPiece :: Maybe Piece -> String
prettyPiece Nothing = " .. "
prettyPiece (Just (Piece c Pawn _ )) = wrap " " $ prettyColor c ++ "P"
prettyPiece (Just (Piece c Bishop _ )) = wrap " " $ prettyColor c ++ "B"
prettyPiece (Just (Piece c Knight _ )) = wrap " " $ prettyColor c ++ "N"
prettyPiece (Just (Piece c Rook _ )) = wrap " " $ prettyColor c ++ "R"
prettyPiece (Just (Piece c King _ )) = wrap " " $ prettyColor c ++ "K"
prettyPiece (Just (Piece c Queen _ )) = wrap " " $ prettyColor c ++ "Q"

-- | Pretty print a color
prettyColor :: PieceColor -> String
prettyColor White = "W"
prettyColor Black = "B"

wrap :: String -> String -> String
wrap border str = border ++ str ++ border
