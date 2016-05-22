module Geometry where

import Data.List (nub)
import Data.Maybe (mapMaybe)

-- | The position of a piece where the first coordinate represents the column
--   and the second represents the row
type Pos = (Int, Int)

-- | Represents a line on the board between 2 points
data Segment = Segment {
        startPoint :: Pos,
        endPoint   :: Pos,
        points     :: [Pos]
    } deriving (Show, Eq)

-- | Adds up 2 positions x1+x2, y1+y2
infixr 9 <+>
(<+>) :: Pos -> Pos -> Pos
(x1, y1) <+> (x2, y2) = (x1+x2, y1+y2)

-- | Adds up 2 position vectors by coordinates and checks if the new
--   position is on the board.
infixr 9 <++>
(<++>) :: Pos -> Pos -> Maybe Pos
p1 <++> p2 =
    let pos' = p1 <+> p2
    in if onBoard pos' then Just pos' else Nothing

-- | Returns True, if the Position is inside the board
onBoard :: Pos -> Bool
onBoard (x, y) = x > 0 && y > 0 && x < 9 && y < 9

-- | Return horizontal + vertical + diagonal segments
allDirectionSegments :: Pos -> [Segment]
allDirectionSegments p = crossSegments p ++ diagonalSegments p

-- | Returns the diagonal segments that can be reached from a given point
diagonalSegments :: Pos -> [Segment]
diagonalSegments p
    | not (onBoard p) = []
    | otherwise =
        let
            ds = diagonals p
            qs = separateQuarters p ds
        in mapMaybe (createSegment p) qs

    where
        separateQuarters :: Pos -> [Pos] -> [[Pos]]
        separateQuarters pp ds =
            filter (not . null) $ concatMap (separateBy pp snd) $ separateBy pp fst ds

-- | Returns the horizontal and vertical segments that can be reached from
--   a given point
crossSegments :: Pos -> [Segment]
crossSegments p =
    let
        cs = cross p
        qs = separateBy p fst cs ++ separateBy p snd cs
    in mapMaybe (createSegment p) qs

-- | Creates a segment based on a starting position and a set of
--   possible ending positions
createSegment :: Pos -> [Pos] -> Maybe Segment
createSegment _ [] = Nothing
createSegment p ps =
    let
        pss = p:ps
        minP = minimum pss
        maxP = maximum pss
    in segment minP maxP

-- | Return every position in a line segment between 2 positions
segment :: Pos -> Pos -> Maybe Segment
segment p1 p2 =
    let m           = slope p1 p2
        (start,end) = order p1 p2
    in  case abs m of
            1.0 -> Just $ Segment start end $ diagonalPoints (round m) start end
            0.0 -> Just $ Segment start end $ hzVtPoints start end
            _   -> Nothing

    where
        -- | Return positions for a section that is diagonal
        diagonalPoints m start end
            | start == end = [start]
            | otherwise = start : diagonalPoints m (start <+> (abs m, m)) end

        -- | Return positions for a section that is horizontal or vertical
        hzVtPoints :: Pos -> Pos -> [Pos]
        hzVtPoints start end
            | start == end = [start]
            | fst start /= fst end = start : hzVtPoints (start <+> (1, 0)) end
            | otherwise = start : hzVtPoints (start <+> (0, 1)) end

        -- | Return 2 positions in order
        order :: Pos -> Pos -> (Pos, Pos)
        order start end
            | start < end = (start, end)
            | otherwise   = (end, start)

-- | Calculate the slope of a line between 2 points
slope :: Pos -> Pos -> Float
slope (x1,y1) (x2,y2) =
    let (xx1,yy1) = (fromIntegral x1, fromIntegral y1)
        (xx2,yy2) = (fromIntegral x2, fromIntegral y2)
    in  if xx2 - xx1 == 0 then 0 else (yy2 - yy1) / (xx2 - xx1)

-- | Return the ambient of an enum
ambient :: (Enum a) => a -> [a]
ambient x = [pred x, x, succ x]

-- | Return the positions around a given position
ambientPos :: Pos -> [Pos]
ambientPos (x,y) =
    [(x',y') | x' <- ambient x, y' <- ambient y, onBoard (x',y')]

-- | Return horizontal + vertical + diagonal positions
allDirections :: Pos -> [Pos]
allDirections p = nub $ cross p ++ diagonals p

-- | Every position in the given row and column
cross :: Pos -> [Pos]
cross (x,y) = nub $ row y ++ column x

-- | Diagonal coordinates relative to the given position
diagonals :: Pos -> [Pos]
diagonals p = nub $ mapMaybe (p <++>) deltas
    where
        deltas = [(dx,dy) | dx <- [-8..8], dy <-[dx,-dx]]

-- | Split a list of positions into 2 lists: one that contains the smaller
--   positions and one that contains the bigger positions compared to the first
--   parameter.
separateBy :: Pos -> (Pos -> Int) -> [Pos] -> [[Pos]]
separateBy p f as =
    let
        lt = filter (\b -> f b < f p) as
        gt = filter (\b -> f b > f p) as
    in [lt, gt]

-- | Return the positions in a row
row :: Int -> [Pos]
row n = [(x, n) | x <- [1..8]]

-- | Return the positions in a row
column :: Int -> [Pos]
column n = [(n, y) | y <- [1..8]]