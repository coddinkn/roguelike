module Monster where

import Tile
import Draw
import Color
import Position
import Entity

import Data.Map hiding (map)
import Data.List

data Monster = Generic { monsterPosition :: Position
                       , monsterStats    :: Stats
                       , monsterChar     :: String
                       }

instance Entity Monster where
    getPosition = monsterPosition
    getStats = monsterStats
    getName _ = "Monster"
    move (Generic pos stats str) dir = Generic newPos stats str
        where newPos = pos + dirToUnitPosition dir
    updateStats monster stats = monster { monsterStats = stats }

instance Show Monster where
    show monster = 
        case monster of
             Generic _ _ monsterChar -> monsterChar

instance Drawable Monster where
    getTiles monster = 
        case monster of
             Generic pos _ _ -> Tiles . fromList $ [(pos, Colored (head . show $ monster) (Green, Black))]

makeMonster :: (Integer, Integer, String) -> Monster
makeMonster (x, y, c) = Generic pos stats c
              where stats = Stats 10 10 10 10 10 10
                    pos = uncurry Grid $ (x, y)

makeMonsters :: String -> String -> String -> [Monster]
makeMonsters xString yString cString = makeMonster <$> zip3 xs ys cs
    where xs = read <$> words xString
          ys = read <$> words yString
          cs = words cString

checkMonsterCollision :: Entity a => [Monster] -> a -> Direction -> Maybe Monster
checkMonsterCollision monsters entity dir = find (samePosition $ move entity dir) monsters
