module Player where

import Entity
import Draw
import Color
import Tile
import Position

import Data.Map

import UI.NCurses

data Player = Player Position

instance Show Player where
    show player = "@"

instance Drawable Player where
    getTiles (Player pos) = fromList $ [(pos, Colored '@' (Yellow, Black))]

instance Entity Player where
    getPosition (Player pos) = pos 
    move (Player pos) = Player . (changePosition pos)
