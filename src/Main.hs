import World
import Draw
import Color
import Action
import Position

import UI.NCurses

import System.Random

eventToInput :: Event -> Maybe Input
eventToInput event = case event of
    EventCharacter c -> Just $ CharKey c
    EventSpecialKey key -> 
        case key of
             KeyUpArrow    -> Just $ SpecialKey UpArrow
             KeyDownArrow  -> Just $ SpecialKey DownArrow
             KeyRightArrow -> Just $ SpecialKey RightArrow
             KeyLeftArrow  -> Just $ SpecialKey LeftArrow
             _ -> Nothing
    _ -> Nothing

getAction :: Window -> Curses Action
getAction window = do
    event <- getEvent window Nothing
    let maybeAction = event >>= eventToInput >>= inputToAction
    case maybeAction of
         Just action -> return action
         _           -> getAction window
    
run :: Window -> World -> Colors -> [Integer] -> Curses ()
run window world colors random = do
    updateWindow window $ do clear
                             draw colors world
    render
    action <- getAction window
    let (newWorld, newRandom) = updateWorld world action random
    if done newWorld
    then closeWindow window 
    else run window newWorld colors newRandom
                                              
main = do world <- loadWorld <$> readFile "world.txt"  
          random <- randoms <$> getStdGen
          runCurses $ do setEcho False
                         setCBreak True
                         setCursorMode CursorInvisible
                         colors <- setUpColorIDs
                         window <- defaultWindow
                         run window world colors random
