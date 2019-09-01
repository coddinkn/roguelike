module Roguelike where

import World
import Entity

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Random
import System.Random
import UI.NCurses

type Roguelike a = WriterT [String] (StateT World (Rand StdGen)) a

roll :: Entity a => (Stats -> Integer) -> (Integer, Integer) -> a -> Roguelike Integer
roll stat range entity = lift . lift $ do rollValue <- getRandomR range
                                          let entityStat = stat $ getStats entity
                                          return $ rollValue + entityStat

logMessage :: String -> Roguelike ()
logMessage = tell . pure

getWorld :: Roguelike World
getWorld = lift get

setWorld :: World -> Roguelike ()
setWorld = lift . put

runRoguelike :: World -> Roguelike () -> Curses (World, [String])
runRoguelike world roguelike =
    do ((_, newLogLines), newWorld) <- liftIO . evalRandIO . flip runStateT world . runWriterT $ roguelike
       return (newWorld, newLogLines)
