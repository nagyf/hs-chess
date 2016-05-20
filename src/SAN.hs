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

-- | Represents annotations that added to moves,
-- like '+' for check and '#' for checkmate
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

-- | A chess move with the annotations attached to it (if any)
type AnnotatedMove = (Move, Maybe Annotation)

-- | An annotated chess move with the necessary disambiguating prefix, if needed
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

-- | Parser for a round, i.e. a white and a black move
round :: Parser [DisambiguatedMove]
round = do
    _ <- roundNumber
    white <- move White
    black <- move Black
    skipMany whitespace >> skipMany eol
    return [white, black]

-- | A parser for round number: e.g. "3."
roundNumber :: Parser Int
roundNumber = do
    digits <- fmap read (many1 digit)
    _ <- char '.' >> skipMany whitespace
    return digits

-- | A parser for a chess move
move :: PieceColor -> Parser DisambiguatedMove
move color =
    try (disambiguatingMove color)
    <|> fmap None (normalMove color)

-- | A parser for a disambiguating chess move (this contains a prefix to denote
-- the source position of the piece, in case of ambiguity)
disambiguatingMove :: PieceColor -> Parser DisambiguatedMove
disambiguatingMove color = do
    from <- try (oneOf columnNames) <|> oneOf ['1'..'8']
    m <- normalMove color
    _ <- skipMany whitespace

    if from `elem` ['1'..'8']
        then return (FromRow (read [from]) m)
        else return (FromColumn (columnNameToInt from) m)

-- A parser for a chess move without ambiguity
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

-- | Parser for castling moves
castling :: PieceColor -> Parser Move
castling color =
    try (queenSideCastle color) <|>
    try (kingSideCastle color)

-- | Parser for king side castling
kingSideCastle :: PieceColor -> Parser Move
kingSideCastle color =
    string "0-0" >> return (KingSideCastle color)

-- | Parser for Queen side castling
queenSideCastle :: PieceColor -> Parser Move
queenSideCastle color =
    string "0-0-0" >> return (QueenSideCastle color)

-- | Parser for pawn promotion
pawnPromotion :: PieceColor -> Parser Move
pawnPromotion color = do
    c <- coordinate
    promoted <- pieceType
    return $ PawnPromotion color promoted c

-- | Parser for a normal pawn move
pawnMove :: PieceColor -> Parser Move
pawnMove color = do
    c <- coordinate
    return $ Move color Pawn c

-- | Parser for an en passant pawn capture
enPassant :: PieceColor -> Parser Move
enPassant color = do
    from <- fmap columnNameToInt columnName
    to <- capturePos
    _ <- string "e.p."
    return $ EnPassant color from to

-- | Parser for a simple capture (except pawn and en passant)
normalCapture :: PieceColor -> Parser Move
normalCapture color = do
    p <- pieceType
    pos <- capturePos
    return $ Capture color p pos

-- | Parser for a pawn capture
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

-- | Parser for an 'x' followed by a position
capturePos :: Parser Pos
capturePos = char 'x' >> coordinate

-- | Parser for a (Int,Int) (i.e. for a board position)
coordinate :: Parser Pos
coordinate = do
    col <- fmap columnNameToInt columnName
    row <- read <$> many1 digit
    return (col, row)

-- | Column name parser
columnName :: Parser Char
columnName = oneOf columnNames

-- | Piece type parser
pieceType :: Parser PieceType
pieceType = do
    ch <- oneOf (Map.keys pieceTypes)
    return $ fromJust $ Map.lookup ch pieceTypes

-- | Annotation parser
annotations :: Parser (Maybe Annotation)
annotations = checkMate <|> check <|> return Nothing

-- | Check annotation parser
check :: Parser (Maybe Annotation)
check = char '+' >> return (Just Check)

-- | Checkmate annotation parser
checkMate :: Parser (Maybe Annotation)
checkMate = char '#' >> return (Just CheckMate)

-- | End of line parser, that supports all line ending formats
eol :: Parser String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

-- | Whitespace (i.e. space and tab) parser
whitespace :: Parser Char
whitespace = space <|> tab

-- | Converts a column label to a number
columnNameToInt :: Char -> Int
columnNameToInt ch = fromJust $ Map.lookup ch columnNameMapping

-- | Returns a map with ('a',1),('b',2)... pairs, to easily map between the
-- column name and it's number representation
columnNameMapping :: Map Char Int
columnNameMapping = Map.fromList $ zip columnNames [1..]

-- | The possible column labels
columnNames :: String
columnNames = ['a'..'h']

-- | Returns a map that contains piece name -> piece type mappings
pieceTypes :: Map Char PieceType
pieceTypes = Map.fromList $
    zip ['N', 'B', 'R', 'Q', 'K'] [Knight, Bishop, Rook, Queen, King]