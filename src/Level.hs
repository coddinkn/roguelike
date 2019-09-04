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

newtype Level = Level { tiles :: Tiles } deriving Show

makeLevel :: [String] -> Level
makeLevel strings = Level . Tiles $ fromList tiles
    where tiles = do (row, string) <- zip [0..] strings
                     (col, char)   <- zip [0..] string
                     return (Grid col row, Colored char (White, Black))
                      
instance Drawable Level where
    getTiles = tiles

getTile :: Level -> Position -> Maybe Char
getTile (Level (Tiles tiles)) pos = head . show <$> lookup pos tiles

checkCollision :: Entity a => Level -> a -> Position -> Bool
checkCollision level entity dpos =
    case maybeTile of
         Just tile -> tile /= '.'
         Nothing   -> False
    where pos = getPosition entity
          maybeTile = getTile level $ pos + dpos
