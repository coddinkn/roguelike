module Tile where

import Color
import Position

import Data.Map

data Tile = Empty
          | Default Char
          | Colored Char (Color, Color)

instance Show Tile where
    show tile = case tile of
        Empty       -> " "
        Default c   -> [c]
        Colored c _ -> [c] 

emptyTile :: Tile -> Bool
emptyTile Empty = True
emptyTile _     = False 

type Tiles = Map Position Tile

layerTiles :: Tiles -> Tiles -> Tiles
layerTiles = union
