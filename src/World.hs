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

data Collision = PlayerCollision Player
               | MonsterCollision Monster
               | LevelCollision
               | NoCollision
               deriving Show

endOfTheWorld :: World -> Bool
endOfTheWorld = not . alive . player

loadWorld :: String -> World
loadWorld contents = World player level monsters
    where x:y:xs:ys:l = lines contents
          playerPos   = Grid (read x :: Integer) (read y :: Integer)
          playerStats = Stats 20 20 12 12 12 12
          player      = Player playerPos playerStats
          monsters    = makeMonsters xs ys
          level       = makeLevel l

checkCollision :: Entity a => World -> a -> Direction -> Collision
checkCollision (World player level monsters) entity dir
    | checkLevelCollision level entity dir = LevelCollision
    | checkPlayerCollision player entity dir = PlayerCollision player
    | otherwise = maybe NoCollision
                        MonsterCollision
                        (checkMonsterCollision monsters entity dir)

updateMonster :: World -> Monster -> (Monster -> Monster) -> World
updateMonster world monster changeMonster =
    let nextMonster = changeMonster monster
        nextMonsters = nextMonster : filter (not . samePosition monster) ms
    in world { monsters = nextMonsters }
    where ms = monsters world

instance Drawable World where
    getTiles (World player level monsters) =
        let monsterTiles = getTiles <$> monsters
            playerTiles  = [getTiles player]
            levelTiles   = [getTiles level]
        in mconcat $ playerTiles ++ monsterTiles ++ levelTiles
