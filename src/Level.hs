module Level where

import Player
import Draw
import Color
import Entity
import Tile
import Position

import Prelude   hiding (lookup)
import Data.List hiding (lookup)
import Data.Map 

data Level = Level { tiles :: Tiles } deriving Show

makeLevel :: [String] -> Level
makeLevel strings = Level . Tiles $ fromList tiles
    where tiles = do (row, string) <- zip [0..] strings
                     (col, char)   <- zip [0..] string
                     return $ (Grid col row, Colored char (White, Black))
                      
instance Drawable Level where
    getTiles = tiles

getTile :: Level -> Integer -> Integer -> Maybe Char
getTile (Level (Tiles tiles)) x y = head . show <$> (lookup (Grid x y) $ tiles)

checkCollision :: Entity a => Level -> a -> Direction -> Bool
checkCollision level entity dir =
    case maybeTile of
         Just tile -> tile /= '.'
         Nothing   -> False
    where pos = getPosition entity
          x = getX pos
          y = getY pos
          maybeTile = getTile level (x + dx) (y + dy)
          (dx, dy)  = case dir of
                           West  -> (-1, 0)
                           East  -> (1,  0)
                           North -> (0, -1)
                           South -> (0,  1)
