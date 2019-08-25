module Input  where

import Position

import UI.NCurses

data Input = Quit
           | Try Direction
           | ScrollLog

getInput :: Window -> Curses Input
getInput window = do
    event <- getEvent window Nothing
    case event >>= eventToInput of
         Just input -> return input
         _          -> getInput window

eventToInput :: Event -> Maybe Input
eventToInput event = case event of
    EventCharacter char -> charToInput char
    EventSpecialKey key -> specialKeyToInput key
    _ -> Nothing
    
specialKeyToInput :: Key -> Maybe Input
specialKeyToInput key = case key of
    KeyLeftArrow  -> Just $ Try West
    KeyDownArrow  -> Just $ Try South
    KeyUpArrow    -> Just $ Try North
    KeyRightArrow -> Just $ Try East
    _ -> Nothing

charToInput :: Char -> Maybe Input
charToInput char = case char of
    'h' -> Just $ Try West
    'j' -> Just $ Try South
    'k' -> Just $ Try North
    'l' -> Just $ Try East
    ' ' -> Just $ ScrollLog
    'q' -> Just Quit 
    _   -> Nothing
