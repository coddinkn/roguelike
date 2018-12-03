module World where

import Action
import Player
import Draw
import Color
import Tile
import Position
import Level
import Monster
import Entity

import UI.NCurses

type World = (Player, Level, Monsters, Bool)

updateWorld :: World -> Action -> World
updateWorld (player, level, monsters, done) action = case action of
    Move dir -> if checkCollision level player dir
                then (player, level, monsters, False) 
                else (move player dir, level, monsters, False)
    Quit     -> (player, level, monsters, True)

loadWorld :: String -> World
loadWorld contents = (player, level, monsters, False)
    where ls           = lines contents 
          x:y:xs:ys:_  = ls
          player       = Player $ Grid (read x :: Integer) (read y :: Integer)
          monsters     = makeMonsters xs ys 
          level        = makeLevel $ drop 4 ls

drawWorld :: Colors -> World -> Update ()
drawWorld colors (player, level, monsters, _) = do
    draw colors level
    mapM_ (draw colors) monsters
    draw colors player

worldDone :: World -> Bool
worldDone (_, _, _, done) = done
