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
import Control.Monad

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
                         let nextMonsters = filter alive $ nextMonster : filter (not . samePosition monster) monsters
                         modifyWorld $ \world -> world { player = nextPlayer, monsters = nextMonsters }

attackPlayer :: Monster -> Roguelike ()
attackPlayer monster = do player <- getPlayer
                          monsters <- getMonsters
                          (nextMonster, nextPlayer) <- fight monster player
                          let nextMonsters = nextMonster : filter (not . samePosition monster) monsters
                          modifyWorld $ \world -> world { player = nextPlayer, monsters = nextMonsters }

monsterTurn :: Monster -> Roguelike ()
monsterTurn monster = do player <- getPlayer
                         world <- getWorld
                         monsters <- getMonsters
                         let maybeDir = monster `dirTowards` player
                         case maybeDir of
                            Just dir -> case checkCollision world monster dir of
                                             PlayerCollision player -> attackPlayer monster
                                             NoCollision -> modifyWorld $ \world -> updateMonster world monster $ flip move dir
                                             _ -> return ()
                            Nothing -> return ()

monstersTurn :: Roguelike ()
monstersTurn = do monsters <- getMonsters
                  mapM_ monsterTurn monsters

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
           then do logMessage $ getName attacker ++ " missed."
                   return (attacker, defender)
           else do logMessage $ getName attacker ++ " hit " ++ (map toLower . getName $ defender) ++ " for " ++ show damage ++ " damage."
                   let newDefender = hurt defender damage
                   unless (alive newDefender) (logMessage $ getName attacker ++ " killed " ++ (map toLower . getName $ defender) ++ ".")
                   return (attacker, newDefender)
