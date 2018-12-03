module Player where

import Entity
import Draw
import Color
import Tile
import Position

import UI.NCurses

data Player = Player Position

instance Show Player where
    show player = "@"

instance Drawable Player where
    getDrawPosition (Player pos) = pos
    getTiles _ = makeTiles (Yellow, Black) "@"

instance Entity Player where
    getPosition = getDrawPosition
    move (Player pos) = Player . (changePosition pos)
