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
                   } deriving Show

updateWorld :: World -> Action -> [Integer] -> (World, [Integer])
updateWorld world action random = (monsterTurn $ playerTurn world action, random)

playerTurn :: World -> Action -> World
playerTurn (World player level monsters done) action = case action of
    Move dir -> if checkCollision level player dir
                then (World player level monsters False) 
                else (World (move player dir) level monsters False)
    Quit     -> World player level monsters True

monsterTurn :: World -> World
monsterTurn = id

loadWorld :: String -> World
loadWorld contents = World player level monsters False
    where ls           = lines contents 
          x:y:xs:ys:_  = ls
          playerPos    = Grid (read x :: Integer) (read y :: Integer)
          playerStats  = Stats 20 20 12 12 12 12
          player       = Player playerPos playerStats
          monsters     = makeMonsters xs ys
          level        = makeLevel $ drop 4 ls

instance Drawable World where
    getTiles (World player level monsters _) = let monsterTiles = getTiles <$> monsters
                                                   playerTiles  = [getTiles player]
                                                   levelTiles   = [getTiles level]
                                               in mconcat $ playerTiles ++ monsterTiles ++ levelTiles
