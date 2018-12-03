import World
import Draw
import Color
import Action
import Position

import UI.NCurses

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
    
run :: Window -> World -> Colors -> Curses ()                   
run window world colors = do 
    updateWindow window $ do clear
                             drawWorld colors world
    render
    action <- getAction window
    let newWorld = updateWorld world action
    if worldDone newWorld
    then closeWindow window 
    else run window newWorld colors
                                              
main = do world <- loadWorld <$> readFile "world.txt"  
          runCurses $ do setEcho False
                         setCBreak True
                         setCursorMode CursorInvisible
                         colors <- setUpColorIDs
                         window <- defaultWindow
                         run window world colors
