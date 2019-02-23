module World where

import Action
import Player
import Draw
import Color
import Tile
import Position
import Level
import Monster
import Entity

data World = World { player :: Player
                   , level :: Level
                   , monsters :: [Monster]
                   , done :: Bool
                   }

updateWorld :: World -> Action -> World
updateWorld (World player level monsters done) action = case action of
    Move dir -> if checkCollision level player dir
                then (World player level monsters False) 
                else (World (move player dir) level monsters False)
    Quit     -> World player level monsters True

loadWorld :: String -> World
loadWorld contents = World player level monsters False
    where ls           = lines contents 
          x:y:xs:ys:_  = ls
          player       = Player $ Grid (read x :: Integer) (read y :: Integer)
          monsters     = makeMonsters xs ys 
          level        = makeLevel $ drop 4 ls

instance Drawable World where
    getTiles (World player level monsters _) = let monsterTiles = foldr1 layerTiles $ getTiles <$> monsters
                                                   entityTiles = layerTiles (getTiles player) monsterTiles
                                               in layerTiles entityTiles $ getTiles level 
