module Monster where

import Tile
import Draw
import Color
import Position

data Monster = Generic Position

type Monsters = [Monster]

instance Show Monster where
    show monster = 
        case monster of
             Generic _ -> "M"

instance Drawable Monster where
    getDrawPosition monster =
        case monster of
             Generic pos -> pos
    getTiles monster = 
        case monster of
             Generic _ -> makeTiles (Green, Black) "M"

makeMonster :: (Integer, Integer) -> Monster
makeMonster (x, y) = Generic $ Grid x y

makeMonsters :: String -> String -> Monsters
makeMonsters xString yString = map makeMonster $ zip xs ys
    where xs = map read $ words xString
          ys = map read $ words yString 
