module Position where

data Direction = East 
               | West 
               | North 
               | South
               | NorthEast
               | NorthWest
               | SouthEast
               | SouthWest
               deriving (Show, Eq)

data Position = Grid { getX :: Integer
                     , getY :: Integer
                     } deriving (Show, Eq, Ord)

liftGrid :: (Integer -> Integer -> Integer) -> Position -> Position -> Position
liftGrid f (Grid x1 y1) (Grid x2 y2) = Grid (f x1 x2) (f y1 y2)

instance Num Position where
    (+) = liftGrid (+)
    (-) = liftGrid (-)
    (*) = liftGrid (*)
    abs (Grid x y) = Grid (abs x) (abs y)
    signum (Grid x y) = Grid (signum x) (signum y)
    fromInteger i = Grid (fromInteger i) (fromInteger i)

getAsPair :: Position -> (Integer, Integer)
getAsPair pos = case pos of
                     Grid x y -> (x, y)

dirToUnitPosition :: Direction -> Position
dirToUnitPosition dir = uncurry Grid $ case dir of
    West  -> (-1,  0)
    East  -> ( 1,  0)
    North -> ( 0, -1)
    South -> ( 0,  1)
    NorthWest -> (-1, -1)
    NorthEast -> ( 1, -1)
    SouthWest -> (-1,  1)
    SouthEast -> ( 1,  1)

changePosition :: Position -> Direction -> Position
changePosition pos dir = pos + change
    where change = dirToUnitPosition dir

distanceToDir :: Position -> Maybe Direction
distanceToDir (Grid x y)
    | x >  0 && y >  0 = Just NorthWest
    | x >  0 && y == 0 = Just West
    | x >  0 && y <  0 = Just SouthWest
    | x == 0 && y >  0 = Just North
    | x == 0 && y == 0 = Nothing
    | x == 0 && y <  0 = Just South
    | x <  0 && y >  0 = Just NorthEast
    | x <  0 && y == 0 = Just East
    | x <  0 && y <  0 = Just SouthEast
