module Action where

import Position

data Input = CharKey    Char
           | SpecialKey Special

data Special = LeftArrow
             | RightArrow
             | UpArrow
             | DownArrow

data Action = Move Direction
            | Quit

inputToAction :: Input -> Maybe Action
inputToAction input = 
    case input of
         CharKey char | char `elem` "hjklq" ->
            Just $ case char of
                        'h' -> Move West
                        'j' -> Move South
                        'k' -> Move North
                        'l' -> Move East
                        'q' -> Quit
         SpecialKey key ->
            Just $ Move $ case key of
                               LeftArrow  -> West
                               RightArrow -> East
                               UpArrow    -> North
                               DownArrow  -> South
         _ -> Nothing
