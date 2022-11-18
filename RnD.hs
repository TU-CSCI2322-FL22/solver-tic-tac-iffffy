module RnD where
import Game
import Solver
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

writeBB :: Maybe Player -> String
writeBB p
  | p == Just Cross = "x"
  | p == Just Circle = "o"
  | p == Nothing = "_" 
  | otherwise = error "what the"

writing :: Turn -> String
writing p
  | p == Cross = "x"
  | p == Circle = "0"
  | otherwise = error "Nothing"
  
readGame :: String -> GameState
readGame str = let lsplit = lines str
                   lineBoards = take 9 lsplit
                   player = last lsplit
               in (reading player, createBigBoard [Game [readingBB char | char <- line] | line <- lineBoards])

loadGame :: FilePath -> IO GameState
loadGame path = do 
  text <- readFile path
  return (readGame text)

showGame :: GameState -> String
showGame (trn, bb) = (intercalate "\n" [concat [writeBB p | p <- mb] | (Game mb) <- snd $ unzip bb]) ++ "\n" ++ (writing trn)

putWinner :: GameState -> IO ()
putWinner gs = putStrLn(show (whoWillWin gs)) 

writeGame::GameState -> FilePath -> IO()
writeGame gas path = writeFile path $ showGame gas

          --- FOR VISUALIZATIONS ---

emptyBoard = replicate 9 (Game $ replicate 9 Nothing)
allXBoard = replicate 9 (Game $ replicate 9 (Just Cross))

--BigBoard, user interface showing
showGameState :: GameState -> String -> String 
showGameState (turn, bboard) message =
  unlines ["Current turn: " ++ show turn, "", showBigBoard bboard, "", message, ""]

 --cell char show
showCell :: Cell -> String
showCell Nothing = "   "
showCell (Just Cross) = " x "
showCell (Just Circle) = " o "

--turn char show
showTurn :: Turn -> String 
showTurn Cross = " x "
showTurn Circle = " o "

showOutcome :: Maybe Outcome -> String
showOutcome (Just (Win Cross)) = " x "
showOutcome (Just (Win Circle)) = " o "
showOutcome (Just (Tie)) = " Tie "

--show show miniboard interface
showMiniBoard :: String -> MiniBoard -> String
showMiniBoard sep (Game cells) =  intercalate sep $ map (intercalate "|") $ chunksOf 3 $ map showCell cells
  
-- showMiniBoard sep (Winner Nothing) =
--   let cells = replicate 9 "Tie"
--   in intercalate sep $ map (intercalate "|") $ chunksOf 3 cells
-- showMiniBoard sep (Winner cell) =
  
--   let cells = replicate 9 $ showCell cell
--   in intercalate sep $ map (intercalate "|") $ chunksOf 3 cells

--should display the big board cells using miniboards
showBigBoard :: BigBoard -> String 
showBigBoard bigBoard =
  let (winners,miniBoards) = unzip bigBoard
      panels = chunksOf 3 miniBoards
      panelSeparator = '\n': replicate 37 '-' ++"\n"
  in intercalate panelSeparator (map printPanel panels) ++ "\n"
  where printPanel :: [MiniBoard] -> String
        printPanel panel =
          intercalate "\n" $ map (intercalate "||") $ transpose $ map ((splitOn "\n") . (showMiniBoard "\n")) panel

--ideas: use lines to separate the different parts of the game state
--insert a string with a turn and bigboard
--bigBoard is a list of miniboards, which is a list of cells
--pseudocode:
--------read game code start--------
{-
          
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
