module Position where

data Direction = East 
               | West 
               | North 
               | South

data Position = Grid { getX :: Integer
                     , getY :: Integer
                     } deriving (Show, Eq, Ord)

instance Semigroup Position where
   Grid x1 y1 <> Grid x2 y2 = Grid x y
      where x = x1 + x2
            y = y1 + y2 

instance Monoid Position where
    mempty  = Grid 0 0

getAsPair :: Position -> (Integer, Integer)
getAsPair pos = case pos of
                     Grid x y -> (x, y)

dirToUnitPosition :: Direction -> Position
dirToUnitPosition dir = uncurry Grid $ case dir of
    West  -> (-1, 0)
    East  -> (1,  0)
    North -> (0, -1)
    South -> (0,  1)

changePosition :: Position -> Direction -> Position
changePosition pos dir = pos <> change
    where change = dirToUnitPosition dir
