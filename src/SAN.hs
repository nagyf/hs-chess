module SAN (parseMoves) where

import Prelude hiding (round)
import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import Data.Maybe
import Data.Map as Map
import Board
import Geometry

data Move = Move PieceColor PieceType Pos
            | Capture PieceColor PieceType Pos
            | PawnCapture PieceColor Int Pos
            | PawnPromotion PieceColor PieceType Pos
            | KingSideCastle PieceColor
            | QueenSideCastle PieceColor
            deriving (Show)

parseMoves :: String -> Maybe [Move]
parseMoves input = case parse match "match" input of
    Left _ -> Nothing
    Right moves -> Just (concatMap snd moves)

match :: Parser [(Int, [Move])]
match = manyTill round eof

round :: Parser (Int, [Move])
round = do
    num <- stepNumber
    whiteMove <- move White
    blackMove <- move Black
    return (num, [whiteMove, blackMove])

stepNumber :: Parser Int
stepNumber = do
    digits <- many1 digit
    separator
    return $ read digits

move :: PieceColor -> Parser Move
move c =
    try (pawnPromotion c) <|>
    try (pawnCapture c) <|>
    try (normalCapture c) <|>
    try (pawnMove c) <|>
    try (normalMove c) <|>
    try (castling c)

normalCapture :: PieceColor -> Parser Move
normalCapture c = do
    p <- pieceTypeParser
    char 'x'
    pos <- coordinate
    endOfMove
    return $ Capture c p pos

pawnCapture :: PieceColor -> Parser Move
pawnCapture c = do
    from <- liftM columnNameToInt $ columnName
    char 'x'
    pos <- coordinate
    endOfMove
    return $ PawnCapture c from pos

normalMove :: PieceColor -> Parser Move
normalMove color = do
    p <- pieceTypeParser
    c <- coordinate
    endOfMove
    return $ Move color p c

pawnMove :: PieceColor -> Parser Move
pawnMove color = do
    c <- coordinate
    endOfMove
    return $ Move color Pawn c

pawnPromotion :: PieceColor -> Parser Move
pawnPromotion color = do
    c <- coordinate
    promoted <- pieceTypeParser
    endOfMove
    return $ PawnPromotion color promoted c

castling :: PieceColor -> Parser Move
castling color =
    try (kingSideCastle color) <|>
    try (queenSideCastle color)

kingSideCastle :: PieceColor -> Parser Move
kingSideCastle color = do
    string "O-O"
    endOfMove
    return $ KingSideCastle color

queenSideCastle :: PieceColor -> Parser Move
queenSideCastle color = do
    string "O-O-O"
    endOfMove
    return $ QueenSideCastle color

pieceTypeParser :: Parser PieceType
pieceTypeParser = do
    ch <- anyChar
    return $ fromJust $ Map.lookup ch pieceTypes

coordinate :: Parser (Int, Int)
coordinate = do
    col <- liftM (columnNameToInt) $ anyChar
    row <- liftM read $ many1 digit
    return (col, row)

columnName :: Parser Char
columnName = oneOf ['a'..'h']

separator :: Parser ()
separator = skipMany1 $ space <|> char '.' <|> endOfLine

endOfLine :: Parser Char
endOfLine = char '\n'

endOfMove :: Parser Char
endOfMove = space <|> endOfLine

columnNames :: Map Char Int
columnNames = Map.fromList $ zip ['a'..'h'] [1..]

pieceTypes :: Map Char PieceType
pieceTypes = Map.fromList $
    zip ['N', 'B', 'R', 'Q', 'K'] [Knight, Bishop, Rook, Queen, King]

columnNameToInt :: Char -> Int
columnNameToInt ch = fromJust $ Map.lookup ch columnNames
