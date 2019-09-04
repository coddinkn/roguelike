module Log where

import Color
import Roguelike

import Prelude hiding (lines)
import Data.List hiding (lines)
import Data.List.Split
import Data.Maybe
import UI.NCurses

data Log = Log { lines        :: [String]
               , currentLine  :: Maybe Integer
               , firstNewLine :: Maybe Integer
               , width        :: Int
               }

emptyLog :: Int -> Log
emptyLog = Log [] Nothing Nothing

scroll :: Log -> Int -> Log
scroll log numLines = log { currentLine = nextLine, firstNewLine = nextNewLine }
             where getNext line 
                         | line == 0 = Nothing 
                         | line >  0 = Just $ line - toInteger numLines
                   nextLine = currentLine log >>= getNext
                   getNextNew line
                         | line == 0 = Nothing
                         | maybe False (> line) $ firstNewLine log = Just $ line - 1
                         | otherwise = firstNewLine log
                   nextNewLine = currentLine log >>= getNextNew

clearFirstNewLine :: Log -> Log
clearFirstNewLine log = log { firstNewLine = Nothing }

top :: Log -> Log
top log = log { currentLine = firstNewLine log } 

addLines :: Log -> [String] -> Log
addLines log newLines = if null newLines
                        then log
                        else log { lines = newLines ++ oldLines, currentLine = nextLine, firstNewLine = nextNewLine }
                        where oldLines = lines log 
                              newLinesLength = toInteger $ length newLines
                              nextLine = Just $ fromMaybe (newLinesLength - 1) $ currentLine log
                              nextNewLine = case firstNewLine log of
                                                 Nothing -> Just $ newLinesLength - 1
                                                 _ -> firstNewLine log

addMessage :: Log -> String -> Log
addMessage log message = addLines log messageLines
    where messageLines = fitToWidth (width log) message

addMessages :: Log -> [String] -> Log
addMessages log messages = addLines log messagesLines
    where messagesLines = fitToWidth (width log) =<< messages

more :: String
more = "... (more)"

addMoreIndicator :: Int -> [String] -> [String]
addMoreIndicator width chunks =
    if length lastChunk <= length more
    then addMore (take (numChunks - 2) chunks)
      ++ [concat  $ drop (numChunks - 2) chunks]
    else addMore (init chunks) ++ [lastChunk]
    where numChunks = length chunks
          lastChunk = last chunks
          addMore = map (++ more)

fitToWidth :: Int -> String -> [String]
fitToWidth width message = reverse $ addMoreIndicator width chunks
  where chunkWidth = width - length more
        chunks = chunksOf chunkWidth message

drawLog :: Colors -> Log -> (Integer, Integer) -> Update Log
drawLog colors log (x, y) = do moveCursor y x
                               setColor $ findColor colors (White, Black)
                               drawString $ replicate (width log) ' '
                               maybe (return $ clearFirstNewLine log)
                                     (\line -> do moveCursor y x
                                                  drawString $ genericIndex (lines log) line
                                                  setColor defaultColorID
                                                  return $ clearFirstNewLine log)
                                     (currentLine log)
