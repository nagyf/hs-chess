module Geometry where

import Data.List (nub)
import Data.Maybe (mapMaybe)

-- | The position of a piece where the first coordinate represents the column
--   and the second represents the row
type Pos = (Int, Int)

-- | Adds up 2 positions
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

-- | Return every position in a line segment between 2 positions
segment :: Pos -> Pos -> [Pos]
segment p1 p2 =
    let m           = slope p1 p2
        (start,end) = order p1 p2
    in  case abs m of
            1.0 -> diagonalPoints (round m) start end
            0.0 -> normalPoints start end
            _   -> []

    where
        -- | Return positions for a section that is diagonal
        diagonalPoints m start end
            | start == end = [start]
            | otherwise = start : diagonalPoints m (start <+> (abs m, m)) end

        -- | Return positions for a section that is horizontal or vertical
        normalPoints :: Pos -> Pos -> [Pos]
        normalPoints start end
            | start == end = [start]
            | fst start /= fst end = start : normalPoints (start <+> (1, 0)) end
            | otherwise = start : normalPoints (start <+> (0, 1)) end

        -- | Return 2 positions in order
        order :: Pos -> Pos -> (Pos, Pos)
        order start end
            | start < end = (start,end)
            | otherwise = (end,start)

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

allDirections :: Pos -> [Pos]
allDirections p = nub $ cross p ++ diagonals p

-- | Every position in the given row and the given column
cross :: Pos -> [Pos]
cross (x,y) = nub $ row y ++ column x

-- | Diagonal coordinates relative to the given position
diagonals :: Pos -> [Pos]
diagonals p = nub $ mapMaybe (p <++>) deltas
    where
        deltas = [(dx,dy) | dx <- [-8..8], dy <-[dx,-dx]]

-- | Return the positions in a row
row :: Int -> [Pos]
row n = [(x, n) | x <- [1..8]]

-- | Return the positions in a row
column :: Int -> [Pos]
column n = [(n, y) | y <- [1..8]]