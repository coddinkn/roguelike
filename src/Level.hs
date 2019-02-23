module Level where

import Action
import Player
import Draw
import Color
import Tile
import Position

import Prelude   hiding (lookup)
import Data.List hiding (lookup)
import Data.Map 

data Level = Level { tiles :: Tiles }

makeLevel :: [String] -> Level
makeLevel strings = Level $ fromList tiles
    where tiles = do (row, string) <- zip [0..] strings
                     (col, char)   <- zip [0..] string
                     return $ (Grid col row, Default char)
                      
instance Drawable Level where
    getTiles = tiles

getTile :: Level -> Integer -> Integer -> Maybe Char
getTile level x y = head . show <$> (lookup (Grid x y) $ tiles level)

checkCollision :: Level -> Player -> Direction -> Bool
checkCollision level (Player pos) dir = 
    case maybeTile of
         Just tile -> tile /= '.'
         Nothing   -> False
    where x = getX pos
          y = getY pos
          maybeTile = getTile level (x + dx) (y + dy)
          (dx, dy)  = case dir of
                           West  -> (-1, 0)
                           East  -> (1,  0)
                           North -> (0, -1)
                           South -> (0,  1)
