import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe
import Debug.Trace

main :: IO ()
main = return ()

data Player = Cross | Circle deriving (Eq)
type Cell = Maybe Player
data MiniBoard = Game [Cell] | Winner (Maybe Player) deriving (Show, Eq)
type BigBoard = [MiniBoard] 
type Turn = Player
-- Cursor parking [ | M | J😩 | K 🥵| R | L | ]
-- Cursor cafe [🍊 ]
type BigBoardIndex = Int
type MiniBoardIndex = Int
type GameState = (Turn, BigBoard)
data Outcome = Win Player | Ongoing BigBoard | Tie deriving (Show, Eq)
type Location = (BigBoardIndex, MiniBoardIndex)

possibleWins = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]

gameStateWinner :: GameState -> Outcome
gameStateWinner = undefined

makeMove :: GameState -> Maybe Location -> GameState
makeMove (Cross, bboard) (Just loc) = -- for human player
  case checkCell loc of 
    True -> undefined
    False -> error "Illegal move"
makeMove (Circle, bboard) Nothing = -- for computer turn
  let loc = bestLoc bboard
  in undefined

  where bestLoc :: BigBoard -> Location
        bestLoc bboard = undefined



getCellOfLocation :: Location -> GameState -> Either Cell Outcome
getCellOfLocation (bigIndex, miniIndex) (_,bboard)
  | bigIndex < 0 || miniIndex < 0 || bigIndex > 8 || miniIndex > 8 = error "IndexOutOfBound in getCellOfLocation"
  | otherwise =
      case (bboard !! bigIndex) of 
        Game cells -> Left $ cells !! miniIndex
        Winner Nothing        -> Right Tie
        Winner (Just player)  -> Right (Win player)

checkCell :: Location -> Bool -- legal move helper
checkCell location = undefined
{-
case cell of Nothing -> let newBoards = updateWinners x board
                          in if thereIsWinner 
                            then celebrate
                            else waitForNewMove
             Just x  -> error "Overwriting move"
  where 
    cell = getCellOfLocation location 
  -}

getLegalMoves :: GameState -> [Location] 
getLegalMoves = undefined

emptyBoard = take 9 $ repeat (Game $ take 9 $ repeat Nothing)
allXBoard = take 9 $ repeat (Game $ take 9 $ repeat (Just Cross))

showGameState :: GameState -> String --BigBoard 
showGameState (turn, bigboard) = unlines ["Current turn: " ++ show turn ++ "\n", showBigBoard bigboard]

instance Show Player where
  show Cross  = " x "
  show Circle = " o "

-- data MiniBoard = Game [Cell] | Winner (Maybe Player) deriving (Show, Eq)

showMiniBoard :: String -> MiniBoard -> String
showMiniBoard sep (Game cells) = 
  intercalate sep $ map (intercalate "|") $ chunksOf 3 $ map showFor cells
  where showFor :: Cell -> String
        showFor Nothing = "   "
        showFor (Just player) = show player
        
showMiniBoard sep (Winner Nothing) = 
  let cells = take 9 $ repeat "Tie"
  in intercalate sep $ map (intercalate "|") $ chunksOf 3 cells

showMiniBoard sep (Winner (Just p)) = 
  let cells = take 9 $ repeat $ show p
  in intercalate sep $ map (intercalate "|") $ chunksOf 3 cells
  
showBigBoard :: BigBoard -> String
showBigBoard miniBoards = 
  let panels = chunksOf 3 miniBoards
      panelSeparator = '\n':(take 37 (repeat '-') ++"\n")
  in (intercalate panelSeparator $ map printPanel panels) ++ "\n"
  where printPanel :: [MiniBoard] -> String
        printPanel panel = 
          intercalate "\n" $ map (intercalate "||") $ transpose $ map (splitOn "\n") $ map (showMiniBoard "\n") panel
      
