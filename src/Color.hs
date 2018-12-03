module Color where

import Data.Map
 
import UI.NCurses hiding (Color)
import qualified UI.NCurses as NC (Color)

data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta 
           | Cyan
           | White
           deriving (Eq, Ord, Show, Enum)

type Colors = Map (Color, Color) ColorID

colorToCursesColor :: Color -> NC.Color
colorToCursesColor color = case color of
     Black   -> ColorBlack
     Red     -> ColorRed
     Green   -> ColorGreen
     Yellow  -> ColorYellow
     Blue    -> ColorBlue
     Magenta -> ColorMagenta
     Cyan    -> ColorCyan
     White   -> ColorWhite

makeColorID :: ((Color, Color), Integer) -> Curses ColorID
makeColorID ((f, b), n) = newColorID foreground background n
    where foreground = colorToCursesColor f
          background = colorToCursesColor b

setUpColorIDs :: Curses Colors 
setUpColorIDs = do
    let colorPairs = (,) <$> [Black ..] <*> [Black ..]
    let pairsWithIds = zip colorPairs [1 ..]
    colorIDs <- mapM makeColorID pairsWithIds
    return $ fromList $ zip colorPairs colorIDs
