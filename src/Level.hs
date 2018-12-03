module Level where

import Action
import Player
import Draw
import Color
import Tile hiding (getTile)
import Position

import Prelude   hiding (lookup)
import Data.List hiding (lookup)
import Data.IntMap ( IntMap
                   , lookup
                   , toAscList
                   , fromList )

data Level = Level (IntMap (IntMap Char))

makeLevel :: [String] -> Level
makeLevel strings = Level $ fromList $ zip [0..] ys
    where ys = map (fromList . zip [0..]) strings

instance Show Level where 
    show (Level ys) = concat $ intersperse "\n" strings
           where strings = map (map snd) $ map toAscList xs
                 xs      = map snd $ toAscList ys :: [IntMap Char]

instance Drawable Level where
    getDrawPosition _ = mempty 
    getTiles = makeTiles (White, Black) . show

getTile :: Level -> Integer -> Integer -> Maybe Char
getTile (Level map) x y = lookup (fromInteger x) map >>= lookup (fromInteger y)

checkCollision :: Level -> Player -> Direction -> Bool
checkCollision level (Player pos) dir = 
    case maybeTile of
         Just tile -> tile /= '.'
         Nothing   -> False
    where x = getX pos
          y = getY pos
          maybeTile = getTile level (y + dy) (x + dx)
          (dx, dy)  = case dir of
                           West  -> (-1, 0)
                           East  -> (1,  0)
                           North -> (0, -1)
                           South -> (0,  1)
