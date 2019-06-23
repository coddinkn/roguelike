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
             Generic pos -> Tiles . fromList $ [(pos, Colored 'M' (Green, Black))]

makeMonster :: (Integer, Integer) -> Monster
makeMonster = Generic . uncurry Grid

makeMonsters :: String -> String -> [Monster]
makeMonsters xString yString = makeMonster <$> zip xs ys
    where xs = read <$> words xString
          ys = read <$> words yString
