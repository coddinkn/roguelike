module Action where

import Roguelike
import Input
import Player
import Monster
import World
import Position
import Entity
import Log

import Control.Monad.Trans

import Data.Maybe

data Action = Move Direction
            | Attack Monster

updateWorld :: World -> Action -> Roguelike World
updateWorld world action = do world1 <- playerTurn world action
                              monsterTurn world1

evaluateInput :: World -> Input -> Maybe Action
evaluateInput world input = case input of
    Try dir | checkPlayerLevelCollision world dir -> Nothing
            | not $ isNothing target -> Attack <$> target
            | otherwise              -> Just $ Move dir
            where target = checkPlayerMonsterCollision world dir
    _ -> Nothing

monsterTurn :: World -> Roguelike World
monsterTurn world = let nextMonsters = filter alive $ monsters world
                    in return $ world { monsters = nextMonsters }

playerTurn :: World -> Action -> Roguelike World
playerTurn world action = case action of
    Move dir -> return $ world { player = move (player world) dir }
    Attack monster -> do (nextPlayer, nextMonster) <- fight (player world) monster
                         let nextMonsters = nextMonster:(filter (not . samePosition monster) (monsters world))
                         return $ world { player = nextPlayer, monsters = nextMonsters }

fight :: (Entity a, Entity b) => a -> b -> Roguelike (a, b)
fight attacker defender =
    do attackerAccuracy <- lift $ accuracyRoll attacker
       attackerAttack   <- lift $ attackRoll   attacker
       defenderEvasion  <- lift $ evasionRoll  defender
       defenderDefense  <- lift $ defenseRoll  defender
       let damage = if attackerAttack > defenderDefense
                    then attackerAttack - defenderDefense
                    else 0
       if defenderEvasion > attackerAccuracy
           then do logMessage "Defender evaded!"
                   return (attacker, defender)
           else do logMessage $ "Attack hit defender for " ++ (show damage) ++ " damage!"
                   return (attacker, hurt defender damage)
