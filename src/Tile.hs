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

newtype Tiles = Tiles (Map Position Tile)

instance Show Tiles where
    show (Tiles tiles) = head . show . snd <$> toList tiles

instance Semigroup Tiles where
    Tiles a <> Tiles b = Tiles $ union a b

instance Monoid Tiles where
    mempty = Tiles empty
