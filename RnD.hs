module RnD where
import Game
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Foldable

readingBB :: Char -> Maybe Player
readingBB str
  | str == 'x' = Just Cross
  | str == 'o' = Just Circle
  | str == '_' = Nothing
  | otherwise = error "what the"

reading :: String -> Turn
reading str
  | str == "x" = Cross
  | str == "o" = Circle
  | otherwise = error "Nothing"

readGame str = let lsplit = lines str
                   lineBoards = take 9 lsplit
                   player = last lsplit
               in (reading player,createBigBoard [Game [readingBB char | char <- line] | line <- lineBoards])
               
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
