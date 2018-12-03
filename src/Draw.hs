module Draw where

import Position
import Tile
import Color

import Data.Map hiding (map)
import Data.Matrix hiding (map)

import UI.NCurses hiding (Color)

drawTiles :: Colors -> Position -> Tiles -> Update ()
drawTiles colors drawPos tiles = mapM_ drawPairs posTilePairs
    where makePairs coord tile = (tileCoordsToPosition drawPos coord, tile)
          posTilePairs = mapPos makePairs tiles
          drawPairs = uncurry (drawTile colors)

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

class (Show a) => Drawable a where
    getDrawPosition :: a -> Position
    getTiles :: a -> Tiles 

draw :: (Drawable a) => Colors -> a -> Update ()
draw colors d = drawTiles colors drawPosition tiles
    where drawPosition = getDrawPosition d
          tiles = getTiles d

layer :: (Drawable a, Drawable b) => a -> b -> Tiles
layer a b = layerTiles (aTiles, aPos) (bTiles, bPos)
    where aTiles = getTiles a
          aPos   = getDrawPosition a
          bTiles = getTiles b
          bPos   = getDrawPosition b
