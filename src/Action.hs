module Action where

import Roguelike
import Input
import Player
import Monster
import Level
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
    Try dir -> case checkCollision world (player world) dir of
                    NoCollision -> Just $ Move dir
                    MonsterCollision monster -> Just $ Attack monster
                    _ -> Nothing
    _ -> Nothing

updateWorld :: Action -> Roguelike ()
updateWorld action = do playerTurn action
                        monstersTurn

playerTurn :: Action -> Roguelike ()
playerTurn action = case action of
    Move dir -> modifyWorld $ \world -> world { player = move (player world) dir }
    Attack monster -> do player <- getPlayer
                         monsters <- getMonsters
                         (nextPlayer, nextMonster) <- fight player monster
                         let nextMonsters = nextMonster : filter (not . samePosition monster) monsters
                         modifyWorld $ \world -> world { player = nextPlayer, monsters = nextMonsters }

attackPlayer :: Monster -> Roguelike ()
attackPlayer monster = do player <- getPlayer
                          monsters <- getMonsters
                          (nextMonster, nextPlayer) <- fight monster player
                          let nextMonsters = nextMonster : filter (not . samePosition monster) monsters
                          modifyWorld $ \world -> world { player = nextPlayer, monsters = nextMonsters }

moveTowardsPlayer :: Monster -> Roguelike ()
moveTowardsPlayer monster = do player <- getPlayer
                               level <- getLevel
                               monsters <- getMonsters
                               let dir = monster `dirTowards` player
                               if maybe True (checkLevelCollision level monster) dir
                                  then return ()
                                  else let nextMonster = maybe monster
                                                               (move monster)
                                                               dir
                                           nextMonsters = nextMonster : filter (not . samePosition monster) monsters
                                       in modifyWorld $ \world -> world { monsters = nextMonsters }

monstersTurn :: Roguelike ()
monstersTurn = do monsters <- getMonsters
                  mapM_ monsterTurn monsters

monsterTurn :: Monster -> Roguelike ()
monsterTurn monster = do player <- getPlayer
                         if attackingDistance monster player
                            then attackPlayer monster
                            else moveTowardsPlayer monster

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
           else do logMessage $ getName attacker ++ " hit " ++ (map toLower . getName $ defender) ++ " for " ++ show damage ++ " damage!"
                   return (attacker, hurt defender damage)
