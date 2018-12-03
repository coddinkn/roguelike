module Tile where

import Color
import Position

import Data.Matrix
import Control.Lens ((^?), element)


-- Tile Coordinates (Row, Column)
type TileCoords = (Int, Int)

tileCoordsToPosition :: Position -> TileCoords -> Position
tileCoordsToPosition basePos (row, column) = basePos <> tilePos
    where x = toInteger $ column - 1
          y = toInteger $ row - 1 
          tilePos = Grid x y

positionToTileCoords :: Position -> TileCoords
positionToTileCoords pos = (row, column)
    where column = fromInteger $ getX pos + 1
          row    = fromInteger $ getY pos + 1

data Tile = Empty
          | Default Char
          | Colored Char (Color, Color)

instance Show Tile where
    show tile = case tile of
        Empty -> " "
        Default c -> [c]
        Colored c _ -> [c] 

emptyTile :: Tile -> Bool
emptyTile Empty = True
emptyTile _     = False 

makeTile :: (Color, Color) -> [[Char]] -> TileCoords -> Tile
makeTile color chars (row, column) = case maybeChar of
    Just char -> Colored char color
    Nothing   -> Empty
    where y = row - 1
          x = column - 1  
          maybeChar = chars ^? element y >>= \xs -> xs ^? element x

type Tiles = Matrix Tile

makeTiles :: (Color, Color) -> String -> Tiles
makeTiles color string = matrix height width (makeTile color ls)
    where ls = lines string
          height = length ls
          width  = maximum $ map length ls

getTile :: Tiles -> TileCoords -> Maybe Tile
getTile tiles (row, column) = safeGet row column tiles

dimensions :: Tiles -> (Integer, Integer)
dimensions tiles = (toInteger $ ncols tiles, toInteger $ nrows tiles)

getBasePosition :: Position -> Position -> Position
getBasePosition pos1 pos2 = Grid x y
    where x = min (getX pos1) (getX pos2)
          y = min (getY pos1) (getY pos2)

getLayeredDimensions :: (Tiles, Position) -> (Tiles, Position) -> (Integer, Integer)
getLayeredDimensions (tTiles, tPos) (bTiles, bPos) = (width, height)
    where (topX,    topY)    = getAsPair tPos
          (bottomX, bottomY) = getAsPair bPos
          (baseX,   baseY)   = (min topX bottomX, min topY bottomY)
          (topW,    topH)    = dimensions tTiles
          (bottomW, bottomH) = dimensions bTiles
          (width,   height)  = (max (topX + topW - baseX) (bottomX + bottomW - baseX), max (topY + topH - baseY) (bottomY + bottomH - baseY))

layerTile :: (Tiles, Position) -> (Tiles, Position) -> TileCoords -> TileCoords -> Tile
layerTile (top, topPos) (bottom, bottomPos) (baseY, baseX) (row, column) = case (topTile, bottomTile) of
    (Just t, _) | not $ emptyTile t -> t
    (_, Just b) | not $ emptyTile b -> b
    _ -> Empty
    where (topRow,    topCol)    = positionToTileCoords topPos
          (bottomRow, bottomCol) = positionToTileCoords bottomPos
          topTile    = getTile top    (row - (topRow - baseY), column - (topCol - baseX)) 
          bottomTile = getTile bottom (row - (bottomRow - baseY), column - (bottomCol - baseX))

layerTiles :: (Tiles, Position) -> (Tiles, Position) -> Tiles
layerTiles (top, topPos) (bottom, bottomPos) = matrix (fromInteger height) (fromInteger width) (layerTile (top, topPos) (bottom, bottomPos) (positionToTileCoords basePos))
    where (width, height) = getLayeredDimensions (top, topPos) (bottom, bottomPos)
          basePos = getBasePosition topPos bottomPos
          (baseX, baseY) = getAsPair basePos
