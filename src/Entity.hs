module Entity where

import Position
import Draw

class (Drawable a) => Entity a where
    getPosition :: a -> Position
    getStats :: a -> Stats
    move :: a -> Direction -> a

data Stats = Stats { currentHealth :: Integer
                   , maxHealth     :: Integer
                   , attack        :: Integer
                   , defense       :: Integer
                   , accuracy      :: Integer
                   , evasion       :: Integer
                   }
