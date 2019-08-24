module Draw where

import Position
import Tile
import Color

import Data.Map hiding (map)

import UI.NCurses hiding (Color)

drawTiles :: Colors -> Tiles -> Update ()
drawTiles colors (Tiles tiles) = mapM_ (uncurry (drawTile colors)) (assocs tiles)

drawTile :: Colors -> Position -> Tile -> Update ()
drawTile colors pos tile = case tile of
    Colored _ drawColor -> do setColor color
                              moveCursor y x
                              drawString $ show tile
                              setColor defaultColorID
        where color = findWithDefault defaultColorID drawColor colors
    _ -> do moveCursor y x
            drawString $ show tile
    where x = getX pos
          y = getY pos

class Drawable a where
    getTiles :: a -> Tiles 

draw :: (Drawable a) => Colors -> a -> Update ()
draw colors drawable = drawTiles colors $ getTiles drawable

layer :: (Drawable a, Drawable b) => a -> b -> Tiles
layer a b = getTiles a <> getTiles b
