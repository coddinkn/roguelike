module Monster where

import Tile
import Draw
import Color
import Position

import Data.Map hiding (map)

data Monster = Generic Position

instance Show Monster where
    show monster = 
        case monster of
             Generic _ -> "M"

instance Drawable Monster where
    getTiles monster = 
        case monster of
             Generic pos -> fromList $ [(pos, Colored 'M' (Green, Black))]

makeMonster :: (Integer, Integer) -> Monster
makeMonster (x, y) = Generic $ Grid x y

makeMonsters :: String -> String -> [Monster]
makeMonsters xString yString = map makeMonster $ zip xs ys
    where xs = map read $ words xString
          ys = map read $ words yString 
