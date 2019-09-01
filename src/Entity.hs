module Entity where

import Position
import Draw

class Drawable a => Entity a where
    getName :: a -> String
    getPosition :: a -> Position
    getStats :: a -> Stats
    move :: a -> Direction -> a
    updateStats :: a -> Stats -> a

data Stats = Stats { currentHealth :: Integer
                   , maxHealth     :: Integer
                   , attack        :: Integer
                   , defense       :: Integer
                   , accuracy      :: Integer
                   , evasion       :: Integer
                   }

alive :: Entity a => a -> Bool
alive a = health > 0
    where health = currentHealth $ getStats a

hurt :: Entity a => a -> Integer -> a
hurt entity damage = updateStats entity $ stats { currentHealth = newHealth }
    where stats = getStats entity
          oldHealth = currentHealth stats
          newHealth = oldHealth - damage

samePosition :: (Entity a, Entity b) => a -> b -> Bool
samePosition a b = aPosition == bPosition
    where aPosition = getPosition a
          bPosition = getPosition b
