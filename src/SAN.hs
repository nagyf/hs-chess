module SAN (
    Move(..),
    DisambiguatedMove(..),
    parseMoves
    ) where

import Prelude hiding (round)
import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Map as Map

import Board hiding (pieceType, color, pos)
import Geometry hiding (row)

data Annotation = Check
                | CheckMate deriving (Show)

-- | Represents a chess move
data Move = Move PieceColor PieceType Pos -- ^ Nd6, e8
            | Capture PieceColor PieceType Pos -- ^ Nxc4
            | PawnCapture PieceColor Int Pos  -- ^ fxa4
            | PawnPromotion PieceColor PieceType Pos -- ^ e8Q
            | KingSideCastle PieceColor -- ^ 0-0
            | QueenSideCastle PieceColor -- ^ 0-0-0
            | EnPassant PieceColor Int Pos -- ^ exd6e.p.
            deriving (Show)

type AnnotatedMove = (Move, [Annotation])

data DisambiguatedMove = None AnnotatedMove
                        | FromRow Int AnnotatedMove
                        | FromColumn Int AnnotatedMove
                        | FromPos Pos AnnotatedMove
                        deriving (Show)

-- | Parse a series of moves from the input string
-- return either an error message or the parsed moves
parseMoves :: String -> Either ParseError [DisambiguatedMove]
parseMoves input = case parse match "match" input of
    Left err -> Left err
    Right moves -> Right moves

-- | Parse a chess match, which consists N rounds
match :: Parser [DisambiguatedMove]
match = concat <$> manyTill round eof

round :: Parser [DisambiguatedMove]
round = do
    _ <- roundNumber
    white <- move White
    black <- move Black
    skipMany whitespace >> skipMany eol
    return [white, black]

roundNumber :: Parser Int
roundNumber = do
    digits <- fmap read (many1 digit)
    _ <- char '.' >> skipMany whitespace
    return digits

move :: PieceColor -> Parser DisambiguatedMove
move color =
    try(disambiguatingMove color)
    <|> fmap None (normalMove color)

disambiguatingMove :: PieceColor -> Parser DisambiguatedMove
disambiguatingMove color = do
    from <- try (oneOf columnNames) <|> oneOf ['1'..'8']
    m <- normalMove color
    _ <- skipMany whitespace

    if from `elem` ['1'..'8']
        then return (FromRow (read [from]) m)
        else return (FromColumn (columnNameToInt from) m)

normalMove :: PieceColor -> Parser AnnotatedMove
normalMove color = do
    m <- try(castling color)
        <|> try (pawnPromotion color)
        <|> try (pawnMove color)
        <|> try (enPassant color)
        <|> try (pawnCapture color)
        <|> try (normalCapture color)
        <|> standardMove color

    as <- annotations
    _ <- skipMany whitespace
    return (m, as)

castling :: PieceColor -> Parser Move
castling color =
    try (queenSideCastle color) <|>
    try (kingSideCastle color)

kingSideCastle :: PieceColor -> Parser Move
kingSideCastle color =
    string "0-0" >> return (KingSideCastle color)

queenSideCastle :: PieceColor -> Parser Move
queenSideCastle color =
    string "0-0-0" >> return (QueenSideCastle color)

pawnPromotion :: PieceColor -> Parser Move
pawnPromotion color = do
    c <- coordinate
    promoted <- pieceType
    return $ PawnPromotion color promoted c

coordinate :: Parser Pos
coordinate = do
    col <- fmap columnNameToInt columnName
    row <- read <$> many1 digit
    return (col, row)

pawnMove :: PieceColor -> Parser Move
pawnMove color = do
    c <- coordinate
    return $ Move color Pawn c

enPassant :: PieceColor -> Parser Move
enPassant color = do
    from <- fmap columnNameToInt columnName
    to <- capturePos
    _ <- string "e.p."
    return $ EnPassant color from to

normalCapture :: PieceColor -> Parser Move
normalCapture color = do
    p <- pieceType
    pos <- capturePos
    return $ Capture color p pos

pawnCapture :: PieceColor -> Parser Move
pawnCapture color = do
    from <- fmap columnNameToInt columnName
    pos <- capturePos
    return $ PawnCapture color from pos

standardMove :: PieceColor -> Parser Move
standardMove color = do
    p <- pieceType
    c <- coordinate
    return $ Move color p c

capturePos :: Parser Pos
capturePos = char 'x' >> coordinate

columnName :: Parser Char
columnName = oneOf columnNames

pieceType :: Parser PieceType
pieceType = do
    ch <- oneOf (Map.keys pieceTypes)
    return $ fromJust $ Map.lookup ch pieceTypes

annotations :: Parser [Annotation]
annotations = checkMate <|> check <|> return []

check :: Parser [Annotation]
check = char '+' >> return [Check]

checkMate :: Parser [Annotation]
checkMate = char '#' >> return [CheckMate]

eol :: Parser String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

whitespace :: Parser Char
whitespace = space <|> tab

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