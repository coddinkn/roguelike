module Game where

import Action
import World
import Color
import Input
import Draw

import System.Random
import Control.Monad.Random
import UI.NCurses
import Data.Maybe

data Game = Game { world  :: World
                 , window :: Window
                 , colors :: Colors
                 , rng    :: StdGen
                 }

initCurses :: Curses (Window, Colors)
initCurses = do setEcho False
                setCBreak True
                setCursorMode CursorInvisible
                window <- defaultWindow
                colors <- setUpColorIDs
                return (window, colors) 

create :: World -> IO Game
create world = do rng <- getStdGen
                  (window, colors) <- runCurses initCurses
                  return $ Game world window colors rng

loop :: Game -> Curses ()
loop (Game world window colors rng) = 
    do updateWindow window $ draw colors world
       render
       input <- getInput window
       case input of
            Quit -> closeWindow window
            _    -> let (nextWorld, nextRng) = maybe (world, rng) 
                                                     (flip runRand rng . updateWorld world)
                                                     maybeAction
                    in loop $ Game nextWorld window colors nextRng
                    where maybeAction = evaluateInput world input

play :: Game -> IO ()
play game = runCurses $ loop game
