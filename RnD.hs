module RnD where
import Game
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Foldable
{-
                      ----- Milestone 2 -----
--simple interface--
readGame :: String -> GameState       --Reads the game state from file
--1) turn 2) BB
--1) turn 2) maybe player 3)minib
--1) Player [2)maybe Player 3)maybe player (list of cells)]
{-
  Sample string:
  Cross\n[(Maybe Cross,[Maybe Cross, Maybe Circle, Maybe Circle])] (which would look like)

  Cross
  (Maybe Cross,[Circle,Cross,Circle,Cross])
-}

reading :: String -> Maybe Player
reading str
  | str == "Cross" = Cross
  | str == "Circle" = Circle
  | otherwise = Nothing

readGame str
  | str == "Cross\n_" = (Cross,[])
  | str == "Circle\n_" = (Circle,[])
  | otherwise = 
    let originGas = lines str
        --head (splitOn ";" str)
        roughturn = reading (head originGas)
        miniB = reading (tail (splitOn ";" originGas))
    in (turn,miniB)

    let maybeP = head (splitOn ";" str)
        miniB = tail (splitOn ";" str)
    in (maybeP,miniB)
-}
--ideas: use lines to separate the different parts of the game state
--insert a string with a turn and bigboard
--bigBoard is a list of miniboards, which is a list of cells
--pseudocode:
--------read game code start--------
{-
readPlayer :: String -> Maybe Player                        --Reads individual player data types
readPlayer "x" = Cross
readPlayer "o" = Circle
readPlayer " " = Nothing

readCell :: String -> Cell   --reads cell?
readCell = undefined

columnHelp :: [String] -> [Player]                    --Applies readPlayer to a column
columnHelp [x] = [readPlayer x]
columnHelp (x:xs) = (readPlayer x): (columnHelp (xs))

readGame :: String -> Maybe [Player]                  --Applies columnHelp to the tail of a line
readGame str = Just (columnHelp (tail mkeStrLst))     --Pattern match at some point (error) 
    where mkeStrLst = splitOn ";" str
          
readFile :: String -> Maybe GameState
readFile str = sequence [readGame x | x <- tail(lines str)] 
----- read game code finish-----------

          
showGame :: GameState -> String       --Shows the file
showGame = undefined

writeGame :: GameState -> FilePath -> IO () --writes game-state from file and converts to IO
writeGame gameState path = writeFile path $ showGameState gameState ""

loadGame :: FilePath -> IO GameState --
--loadGame path = loadGame writeGame >>= print 
loadGame path = undefined



------
putWinner :: GameState -> IO () --computes and prints winning move
putWinner = undefined
-}
