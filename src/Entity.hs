module Entity where

import Position
import Draw

class (Drawable a) => Entity a where
    getPosition :: a -> Position
    move :: a -> Direction -> a
