module Action where

import Input
import Player
import Monster
import World
import Position
import Entity

import System.Random
import Control.Monad.Random
import Data.Maybe

data Action = Move Direction
            | Attack Monster

updateWorld :: World -> Action -> Rand StdGen World
updateWorld world action = monsterTurn <$> playerTurn world action

evaluateInput :: World -> Input -> Maybe Action
evaluateInput world input = case input of
    Try dir | checkPlayerLevelCollision world dir -> Nothing
            | not $ isNothing target -> Attack <$> target
            | otherwise              -> Just $ Move dir
            where target = checkPlayerMonsterCollision world dir
    _ -> Nothing

monsterTurn :: World -> World
monsterTurn world = let nextMonsters = filter alive $ monsters world
                    in world { monsters = nextMonsters }

playerTurn :: World -> Action -> Rand StdGen World
playerTurn world action = case action of
    Move dir -> return $ world { player = move (player world) dir }
    Attack monster -> do (nextPlayer, nextMonster) <- fight (player world) monster
                         let nextMonsters = nextMonster:(filter (not . samePosition monster) (monsters world))
                         return $ world { player = nextPlayer, monsters = nextMonsters }
