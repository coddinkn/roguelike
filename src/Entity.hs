module Entity where

import Position
import Draw

import System.Random
import Control.Monad.Random

class Drawable a => Entity a where
    getPosition :: a -> Position
    getStats :: a -> Stats
    move :: a -> Direction -> a
    updateStats :: a -> Stats -> a

data Stats = Stats { currentHealth :: Integer
                   , maxHealth     :: Integer
                   , attack        :: Integer
                   , defense       :: Integer
                   , accuracy      :: Integer
                   , evasion       :: Integer
                   }

roll :: Entity a => (Stats -> Integer) -> (Integer, Integer) -> a -> Rand StdGen Integer
roll stat range entity = do rollValue <- getRandomR range
                            let entityStat = stat $ getStats entity
                            return $ rollValue + entityStat

evasionRoll :: Entity a => a -> Rand StdGen Integer
evasionRoll = roll evasion (-4, 1)

attackRoll :: Entity a => a -> Rand StdGen Integer
attackRoll = roll attack (-1, 2)

accuracyRoll :: Entity a => a -> Rand StdGen Integer
accuracyRoll = roll accuracy (-1, 6)

defenseRoll :: Entity a => a -> Rand StdGen Integer
defenseRoll = roll defense (-2, 1)

hurt :: Entity a => a -> Integer -> a
hurt entity damage = updateStats entity $ stats { currentHealth = newHealth }
    where stats = getStats entity
          oldHealth = currentHealth stats
          newHealth = oldHealth - damage

fight :: (Entity a, Entity b) => a -> b -> Rand StdGen (a, b)
fight attacker defender =
    do attackerAccuracy <- accuracyRoll attacker
       attackerAttack   <- attackRoll   attacker
       defenderEvasion  <- evasionRoll  defender
       defenderDefense  <- defenseRoll  defender
       let damage = if attackerAttack > defenderDefense
                    then attackerAttack - defenderDefense
                    else 0
       return $ if defenderEvasion > attackerAccuracy
                    then (attacker, defender)
                    else (attacker, hurt defender damage)
