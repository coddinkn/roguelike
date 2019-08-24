module World where

import Entity
import Player
import Draw
import Color
import Tile
import Position
import Level
import Monster

import Data.List

data World = World { player :: Player
                   , level :: Level
                   , monsters :: [Monster]
                   } deriving Show

loadWorld :: String -> World
loadWorld contents = World player level monsters
    where x:y:xs:ys:l = lines contents
          playerPos   = Grid (read x :: Integer) (read y :: Integer)
          playerStats = Stats 20 20 12 12 12 12
          player      = Player playerPos playerStats
          monsters    = makeMonsters xs ys
          level       = makeLevel l

checkPlayerLevelCollision :: World -> Direction -> Bool
checkPlayerLevelCollision (World player level _) = checkCollision level player

checkPlayerMonsterCollision :: World -> Direction -> Maybe Monster
checkPlayerMonsterCollision (World player _ monsters) dir = find samePosition monsters
    where samePosition monster = getPosition monster == (getPosition player + dirToUnitPosition dir)

instance Drawable World where
    getTiles (World player level monsters) =
        let monsterTiles = getTiles <$> monsters
            playerTiles  = [getTiles player]
            levelTiles   = [getTiles level]
        in mconcat $ playerTiles ++ monsterTiles ++ levelTiles
