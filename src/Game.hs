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
                 }

initCurses :: Curses (Window, Colors)
initCurses = do setEcho False
                setCBreak True
                setCursorMode CursorInvisible
                window <- defaultWindow
                colors <- setUpColorIDs
                updateWindow window $ fillScreen colors (White, Black)
                render
                return (window, colors) 

create :: World -> IO Game
create world = do (window, colors) <- runCurses initCurses
                  return $ Game world window colors

loop :: Game -> Curses ()
loop (Game world window colors) =
    do updateWindow window $ draw colors world
       render
       input <- getInput window
       case input of
            Quit -> closeWindow window
            _    -> do nextWorld <- maybe (return world)
                                          (liftIO . evalRandIO . updateWorld world)
                                          maybeAction
                       loop $ Game nextWorld window colors
                       where maybeAction = evaluateInput world input

play :: Game -> IO ()
play game = runCurses $ loop game
