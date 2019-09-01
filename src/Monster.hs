module Monster where

import Tile
import Draw
import Color
import Position
import Entity

import Data.Map hiding (map)

data Monster = Generic { monsterPosition :: Position
                       , monsterStats    :: Stats
                       }

instance Entity Monster where
    getPosition = monsterPosition
    getStats = monsterStats
    getName _ = "Monster"
    move (Generic pos stats) dir = Generic newPos stats
                                   where newPos = changePosition pos dir
    updateStats monster stats = monster { monsterStats = stats }

instance Show Monster where
    show monster = 
        case monster of
             Generic _ _ -> "M"

instance Drawable Monster where
    getTiles monster = 
        case monster of
             Generic pos _ -> Tiles . fromList $ [(pos, Colored (head . show $ monster) (Green, Black))]

makeMonster :: (Integer, Integer) -> Monster
makeMonster = flip Generic stats . uncurry Grid
              where stats = Stats 10 10 10 10 10 10

makeMonsters :: String -> String -> [Monster]
makeMonsters xString yString = makeMonster <$> zip xs ys
    where xs = read <$> words xString
          ys = read <$> words yString
