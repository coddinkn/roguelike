import World
import Game

main = do world <- loadWorld <$> readFile "world.txt"  
          game <- create world
          play game
