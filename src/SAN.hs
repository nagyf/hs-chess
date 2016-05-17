module SAN (
    Move(..),
    parseMoves
    ) where

import Prelude hiding (round)
import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Map as Map

import Board hiding (pieceType, color, pos)
import Geometry hiding (row)

-- | Represents a chess move
data Move = Move PieceColor PieceType Pos
            | Capture PieceColor PieceType Pos
            | PawnCapture PieceColor Int Pos
            | PawnPromotion PieceColor PieceType Pos
            | KingSideCastle PieceColor
            | QueenSideCastle PieceColor
            deriving (Show)

-- | Parse a series of moves from the input string
-- return either an error message or the parsed moves
parseMoves :: String -> Either String [Move]
parseMoves input = case parse match "match" input of
    Left m -> Left (show m)
    Right moves -> Right (concatMap snd moves)

-- | Parse a chess match, which consists N rounds
match :: Parser [(Int, [Move])]
match = manyTill round eof

-- | A round is a white move followed by a black move
round :: Parser (Int, [Move])
round = do
    num <- roundNumber
    whiteMove <- move White
    blackMove <- move Black
    return (num, [whiteMove, blackMove])

-- | Parser for the number of the round
roundNumber :: Parser Int
roundNumber = do
    digits <- many1 digit
    separator
    return $ read digits

-- | Parser for a single move
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
    p <- pieceType
    pos <- capturePos
    return $ Capture c p pos

pawnCapture :: PieceColor -> Parser Move
pawnCapture c = do
    from <- fmap columnNameToInt columnName
    pos <- capturePos
    return $ PawnCapture c from pos

capturePos :: Parser Pos
capturePos = do
    _ <- char 'x'
    pos <- coordinate
    _ <- endOfMove
    return pos

normalMove :: PieceColor -> Parser Move
normalMove color = do
    p <- pieceType
    c <- coordinate
    _ <- endOfMove
    return $ Move color p c

pawnMove :: PieceColor -> Parser Move
pawnMove color = do
    c <- coordinate
    _ <- endOfMove
    return $ Move color Pawn c

pawnPromotion :: PieceColor -> Parser Move
pawnPromotion color = do
    c <- coordinate
    promoted <- pieceType
    _ <- endOfMove
    return $ PawnPromotion color promoted c

castling :: PieceColor -> Parser Move
castling color =
    try (kingSideCastle color) <|>
    try (queenSideCastle color)

kingSideCastle :: PieceColor -> Parser Move
kingSideCastle color =
    string "O-O" >> endOfMove >> return (KingSideCastle color)

queenSideCastle :: PieceColor -> Parser Move
queenSideCastle color =
    string "O-O-O" >> endOfMove >> return (QueenSideCastle color)

pieceType :: Parser PieceType
pieceType = do
    ch <- anyChar
    return $ fromJust $ Map.lookup ch pieceTypes

coordinate :: Parser (Int, Int)
coordinate = do
    col <- fmap columnNameToInt anyChar
    row <- read <$> many1 digit
    return (col, row)

columnName :: Parser Char
columnName = oneOf ['a'..'h']

separator :: Parser ()
separator = skipMany1 $ space <|> char '.' <|> endOfLine

endOfLine :: Parser Char
endOfLine = char '\n'

endOfMove :: Parser Char
endOfMove = space <|> endOfLine

-- | Converts a column label to a number
columnNameToInt :: Char -> Int
columnNameToInt ch = fromJust $ Map.lookup ch columnNames

-- | Returns a map with ('a',1),('b',2)... pairs, to easily map between the
-- column name and it's number representation
columnNames :: Map Char Int
columnNames = Map.fromList $ zip ['a'..'h'] [1..]

-- | Returns a map that contains piece name -> piece type mappings
pieceTypes :: Map Char PieceType
pieceTypes = Map.fromList $
    zip ['N', 'B', 'R', 'Q', 'K'] [Knight, Bishop, Rook, Queen, King]
