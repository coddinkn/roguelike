module Player where

import Entity
import Draw
import Color
import Tile
import Position

import Data.Map

import UI.NCurses

data Player = Player { position :: Position
                     , stats    :: Stats
                     }

instance Show Player where
    show player = "@"

instance Drawable Player where
    getTiles (Player pos _) = Tiles . fromList $ [(pos, Colored '@' (Yellow, Black))]

instance Entity Player where
    getPosition = position
    move player dir = player { position = newPos }
         where pos = position player
               newPos = changePosition pos dir
    getStats = stats
    updateStats player s= player { stats = s }
