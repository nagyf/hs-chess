module SAN (
    Move(..),
    parseMoves
    ) where

import Prelude hiding (round)
import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Map as Map
import Control.Monad (void)

import Board hiding (pieceType, color, pos)
import Geometry hiding (row)

data MoveFlag = Check
                | CheckMate deriving (Show)

-- | Represents a chess move
data Move = Move PieceColor PieceType Pos [MoveFlag]
            | Capture PieceColor PieceType Pos [MoveFlag]
            | PawnCapture PieceColor Int Pos [MoveFlag]
            | PawnPromotion PieceColor PieceType Pos [MoveFlag]
            | KingSideCastle PieceColor [MoveFlag]
            | QueenSideCastle PieceColor [MoveFlag]
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
move color =
    try (pawnPromotion color) <|>
    try (pawnCapture color) <|>
    try (pawnMove color) <|>
    try (normalCapture color) <|>
    try (normalMove color) <|>
    try (castling color)

normalCapture :: PieceColor -> Parser Move
normalCapture color = do
    p <- pieceType
    pos <- capturePos
    flags <- endOfMove
    return $ Capture color p pos flags

pawnCapture :: PieceColor -> Parser Move
pawnCapture color = do
    from <- fmap columnNameToInt columnName
    pos <- capturePos
    flags <- endOfMove
    return $ PawnCapture color from pos flags

capturePos :: Parser Pos
capturePos = char 'x' >> coordinate

normalMove :: PieceColor -> Parser Move
normalMove color = do
    p <- pieceType
    c <- coordinate
    flags <- endOfMove
    return $ Move color p c flags

pawnMove :: PieceColor -> Parser Move
pawnMove color = do
    c <- coordinate
    flags <- endOfMove
    return $ Move color Pawn c flags

pawnPromotion :: PieceColor -> Parser Move
pawnPromotion color = do
    c <- coordinate
    promoted <- pieceType
    flags <- endOfMove
    return $ PawnPromotion color promoted c flags

castling :: PieceColor -> Parser Move
castling color =
    try (kingSideCastle color) <|>
    try (queenSideCastle color)

kingSideCastle :: PieceColor -> Parser Move
kingSideCastle color =
    string "O-O" >> endOfMove >>= fmap return (KingSideCastle color)

queenSideCastle :: PieceColor -> Parser Move
queenSideCastle color =
    string "O-O-O" >> endOfMove >>= fmap return (QueenSideCastle color)

pieceType :: Parser PieceType
pieceType = do
    ch <- oneOf (Map.keys pieceTypes)
    return $ fromJust $ Map.lookup ch pieceTypes

coordinate :: Parser Pos
coordinate = do
    col <- fmap columnNameToInt columnName
    row <- read <$> many1 digit
    return (col, row)

columnName :: Parser Char
columnName = oneOf columnNames

separator :: Parser ()
separator = skipMany1 (space <|> char '.') >> skipMany endOfLine

endOfLine :: Parser ()
endOfLine =
    void (try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r")

endOfMove :: Parser [MoveFlag]
endOfMove = try eomWithFlags <|> eomWithoutFlags
    where
        eomWithoutFlags :: Parser [MoveFlag]
        eomWithoutFlags = skipMany space >> skipMany endOfLine >> return []

        eomWithFlags :: Parser [MoveFlag]
        eomWithFlags = do
            flags <- flagParser
            skipMany space >> skipMany endOfLine
            return flags

flagParser :: Parser [MoveFlag]
flagParser = do
    flag <- oneOf ['+', '#']
    case flag of
        '+' -> return [Check]
        '#' -> return [CheckMate]
        _   -> return []

-- | Converts a column label to a number
columnNameToInt :: Char -> Int
columnNameToInt ch = fromJust $ Map.lookup ch columnNameMapping

-- | Returns a map with ('a',1),('b',2)... pairs, to easily map between the
-- column name and it's number representation
columnNameMapping :: Map Char Int
columnNameMapping = Map.fromList $ zip columnNames [1..]

columnNames :: String
columnNames = ['a'..'h']

-- | Returns a map that contains piece name -> piece type mappings
pieceTypes :: Map Char PieceType
pieceTypes = Map.fromList $
    zip ['N', 'B', 'R', 'Q', 'K'] [Knight, Bishop, Rook, Queen, King]
