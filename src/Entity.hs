module Entity where

import Position
import Draw

class (Drawable a) => Entity a where
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

roll :: (Integer, Integer) -> Integer -> Integer
roll (low, high) random = (random `mod` n) + low
    where n = (high - low) + 1

statRoll :: (Entity a) => a -> (Stats -> Integer) -> (Integer, Integer) -> Integer -> Integer
statRoll entity stat range = (+) (stat . getStats $ entity) . roll range

evasionRoll  entity = statRoll entity evasion  (-4, 1)
attackRoll   entity = statRoll entity attack   (-1, 2)
accuracyRoll entity = statRoll entity accuracy (-1, 6)
defenseRoll  entity = statRoll entity defense  (-2, 1)

hurt :: Entity a => a -> Integer -> a
hurt entity damage = updateStats entity $ stats { currentHealth = newHealth }
    where stats = getStats entity
          oldHealth = currentHealth stats
          newHealth = oldHealth - damage

fight :: (Entity a, Entity b) => [Integer] -> a -> b -> (a, b, [Integer])
fight (r1:r2:r3:r4:newRandom) attacker defender = if defenderEvasion > attackerAccuracy
                                          then (attacker, defender, newRandom)
                                          else (attacker, hurt defender damage, newRandom)
    where attackerAccuracy = accuracyRoll attacker r1
          attackerAttack   = attackRoll   attacker r2
          defenderEvasion  = evasionRoll  defender r3
          defenderDefense  = defenseRoll  defender r4
          damage = if attackerAttack > defenderDefense
                   then attackerAttack - defenderDefense
                   else 0
