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
monsterTurn = id

playerTurn :: World -> Action -> Rand StdGen World
playerTurn world action = return $
    case action of
         Move dir -> world { player = move (player world) dir }
         _ -> world
