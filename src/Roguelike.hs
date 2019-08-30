module Roguelike where

import World

import Control.Monad.Writer
import Control.Monad.Random
import System.Random
import UI.NCurses

type Roguelike a = WriterT [String] (Rand StdGen) a

runRoguelike :: Roguelike a -> Curses (a, [String])
runRoguelike = liftIO . evalRandIO . runWriterT
