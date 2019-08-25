module Game where

import Action
import World
import Color
import Input
import Draw
import Log

import System.Random
import Control.Monad.Random
import UI.NCurses
import Data.Maybe

data Game = Game { world  :: World
                 , window :: Window
                 , colors :: Colors
                 , log    :: Log
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
                  let log = addMessage (emptyLog 80) "Welcome to roguelike!"
                  return $ Game world window colors log

loop :: Game -> Curses ()
loop (Game world window colors log) =
    do newLog <- updateWindow window $ do draw colors world
                                          newLog <- drawLog colors log (0, 15)
                                          return newLog
       render
       input <- getInput window
       case input of
            Quit -> closeWindow window
            ScrollLog -> loop $ Game world window colors $ scroll newLog 1
            _    -> do nextWorld <- maybe (return world)
                                          (liftIO . evalRandIO . updateWorld world)
                                          maybeAction
                       loop $ Game nextWorld window colors $ top newLog
                       where maybeAction = evaluateInput world input

play :: Game -> IO ()
play game = runCurses $ loop game
