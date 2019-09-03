module Action where

import Roguelike
import Input
import Player
import Monster
import World
import Position
import Entity
import Log

import Data.Maybe
import Data.Char

data Action = Move Direction
            | Attack Monster

evaluateInput :: World -> Input -> Maybe Action
evaluateInput world input = case input of
    Try dir | checkPlayerLevelCollision world dir -> Nothing
            | not $ isNothing target -> Attack <$> target
            | otherwise              -> Just $ Move dir
            where target = checkPlayerMonsterCollision world dir
    _ -> Nothing

updateWorld :: Action -> Roguelike ()
updateWorld action = do playerTurn action
                        monsterTurn

playerTurn :: Action -> Roguelike ()
playerTurn action = case action of
    Move dir -> modifyWorld $ \world -> world { player = move (player world) dir }
    Attack monster -> do player <- getPlayer
                         monsters <- getMonsters
                         (nextPlayer, nextMonster) <- fight player monster
                         let nextMonsters = nextMonster:(filter (not . samePosition monster) monsters)
                         modifyWorld $ \world -> world { player = nextPlayer, monsters = nextMonsters }

monsterTurn :: Roguelike ()
monsterTurn = modifyWorld $ \world -> let nextMonsters = filter alive $ monsters world
                                      in world { monsters = nextMonsters }

evasionRoll :: Entity a => a -> Roguelike Integer
evasionRoll = roll evasion (-4, 1)

attackRoll :: Entity a => a -> Roguelike Integer
attackRoll = roll attack (-1, 2)

accuracyRoll :: Entity a => a -> Roguelike Integer
accuracyRoll = roll accuracy (-1, 6)

defenseRoll :: Entity a => a -> Roguelike Integer
defenseRoll = roll defense (-2, 1)

fight :: (Entity a, Entity b) => a -> b -> Roguelike (a, b)
fight attacker defender =
    do attackerAccuracy <- accuracyRoll attacker
       attackerAttack   <- attackRoll   attacker
       defenderEvasion  <- evasionRoll  defender
       defenderDefense  <- defenseRoll  defender
       let damage = if attackerAttack > defenderDefense
                    then attackerAttack - defenderDefense
                    else 0
       if defenderEvasion > attackerAccuracy
           then do logMessage $ getName defender ++ " evaded!"
                   return (attacker, defender)
           else do logMessage $ getName attacker ++ " hit " ++ (map toLower . getName $ defender) ++ " for " ++ (show damage) ++ " damage!"
                   return (attacker, hurt defender damage)
