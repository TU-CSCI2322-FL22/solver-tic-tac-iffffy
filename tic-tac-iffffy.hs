import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

main :: IO ()
main = return ()

data Player = Cross | Circle deriving (Show, Eq)
type Cell = Maybe Player
data MiniBoard = Game [Cell] | Winner (Maybe Player) deriving (Show, Eq)
type BigBoard = [MiniBoard] 
type Turn = Player
-- Cursor parking [ | M | J😩 | K 🥵| R | L | ]
-- Cursor cafe [🍊 ]
type BigBoardIndex = Int
type MiniBoardIndex = Int
type GameState = (Turn, BigBoard)
data Outcome = Win Player | Ongoing BigBoard | Tie deriving (Eq)
type Location = (BigBoardIndex, MiniBoardIndex)

possibleWins = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]

squaresFor :: Player ->  MiniBoard -> [Int]
squaresFor player (Game mb) = [ loc | (loc, piece) <- zip [0..] mb, piece == Just player]
squaresFor player (Winner p) = error "Shouldn't reach here"

miniWinner :: Player -> MiniBoard -> Bool
miniWinner player mb = didIWin (squaresFor player mb)

didIWin :: [Int] -> Bool
didIWin indices = 
  any (all (`elem` indices)) possibleWins

winnersFor :: Player -> BigBoard -> [Int]
winnersFor player bb = [ loc | (loc, piece) <- zip [0..] bb, miniWinner player piece ]

gameStateWinner :: GameState -> Outcome
gameStateWinner (turn, bigBoard) 
  | didIWin (winnersFor turn bigBoard) = Win turn
  | didIWin (winnersFor (anotherTurn turn) bigBoard) = Win (anotherTurn turn)
  | otherwise  = Ongoing bigBoard -- How to determine Tie

{-
updateMatrix m x (r,c) =
  let (Game cellsAtC) = m !! r 
  in take r m ++
    [Game $ take (c-1) cellsAtC ++ [Just x] ++ drop (c + 1) cellsAtC] ++
    drop (r + 1) m
-}

updateMatrix :: BigBoard -> Turn -> Location -> BigBoard
updateMatrix m x (r,c) = 
  case (splitAt r m) of
       (start, (Game cellsAtC):rest) -> start ++ [Game $ take (c-1) cellsAtC ++ [Just x] ++ drop (c + 1) cellsAtC]  ++ rest
       _ -> error "invalid updateMatrix, should never happen."

makeMove :: GameState -> Location -> Maybe GameState
makeMove (turn, bboard) loc = 
  if checkCell loc (turn, bboard) then Just (anotherTurn turn, updateMatrix bboard Cross loc)
  else Nothing --error "Illegal move"

anotherTurn :: Turn -> Turn
anotherTurn Cross  = Circle
anotherTurn Circle = Cross
        

-- makeMove (Circle, bboard) loc = -- for computer turn
--   if checkCell loc (Circle, bboard) then (Cross , updateMatrix bboard Circle loc)
--     else error "Illegal move"


getCellOfLocation :: Location -> GameState -> Either Cell Outcome --needs to return maybe for check cell
getCellOfLocation (bigIndex, miniIndex) (_,bboard)
  | bigIndex < 0 || miniIndex < 0 || bigIndex > 8 || miniIndex > 8 = error "IndexOutOfBound in getCellOfLocation"
  | otherwise =
      case (bboard !! bigIndex) of 
        Game cells -> Left $ cells !! miniIndex
        Winner Nothing        -> Right Tie
        Winner (Just player)  -> Right (Win player)

checkCell :: Location -> GameState -> Bool -- do bounds checking in here to return false so code doesnt crash
checkCell location gs = 
  case getCellOfLocation location gs of 
          Right _ -> False
          Left (Nothing) -> True
          Left (Just player) -> False

getLegalMoves :: GameState -> [Location] 
getLegalMoves gas = [(x,y) | x <- [0..8], y <- [0..8], x < y, checkCell (x,y) gas] 

  

          --- FOR VISUALIZATIONS ---

emptyBoard = take 9 $ repeat (Game $ take 9 $ repeat Nothing)
allXBoard = take 9 $ repeat (Game $ take 9 $ repeat (Just Cross))

showGameState :: GameState -> String -> String --BigBoard 
showGameState (turn, bboard) message = 
  unlines ["Current turn: " ++ show turn, "", showBigBoard bboard, "", message, ""]

showCell :: Cell -> String
showCell Nothing = "   "
showCell (Just Cross) = " x "
showCell (Just Circle) = " o "

showTurn :: Turn -> String
showTurn Cross = " x "
showTurn Circle = " o "

-- data MiniBoard = Game [Cell] | Winner (Maybe Player) deriving (Show, Eq)

showMiniBoard :: String -> MiniBoard -> String
showMiniBoard sep (Game cells) = 
  intercalate sep $ map (intercalate "|") $ chunksOf 3 $ map showCell cells

showMiniBoard sep (Winner Nothing) = 
  let cells = replicate 9 "Tie"
  in intercalate sep $ map (intercalate "|") $ chunksOf 3 cells

showMiniBoard sep (Winner cell) = 
  let cells = replicate 9 $ showCell cell
  in intercalate sep $ map (intercalate "|") $ chunksOf 3 cells
  
showBigBoard :: BigBoard -> String
showBigBoard miniBoards = 
  let panels = chunksOf 3 miniBoards
      panelSeparator = '\n': replicate 37 '-' ++"\n"
  in (intercalate panelSeparator $ map printPanel panels) ++ "\n"
  where printPanel :: [MiniBoard] -> String
        printPanel panel = 
          intercalate "\n" $ map (intercalate "||") $ transpose $ map (splitOn "\n") $ map (showMiniBoard "\n") panel
