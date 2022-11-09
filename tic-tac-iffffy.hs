import Data.List
import Data.List.Split
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
data Outcome = Win Player | Ongoing | Tie deriving (Eq) -- ongoing is property of bigboard
type Location = (BigBoardIndex, MiniBoardIndex)

possibleWins = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]

xSquares :: Player -> MiniBoard -> [Int]
xSquares pl mb = [loc | (loc, piece) < zip [0..]mb, piece == Just pl]

xWins miniBoard = any (`subseteq` (squareFor X miniboard)) possibleWins

winnersFor :: Player -> Bigboard -> [Int]
--just like squaresFor, but check for squares the player has won



gameStateWinner :: GameState -> Outcome
gameStateWinner = undefined

updateMatrix :: BigBoard -> Turn -> Location -> BigBoard
updateMatrix m x (r,c) =
  let (Game cellsAtC) = m !! r 
  in take r m ++
    [Game $ take (c-1) cellsAtC ++ [Just x] ++ drop (c + 1) cellsAtC] ++
    drop (r + 1) m

makeMove :: GameState -> Location -> GameState -- made a few edits, removed maybe and added nothing
makeMove (Cross, bboard) (Just loc) = -- for human player
  case checkCell loc (Cross, bboard) of 
    True -> (Circle, updateMatrix bboard Cross loc)
    False -> Nothing
makeMove (Circle, bboard) Nothing = -- for other turn
  case checkCell loc (Circle, bboard) of 
    True -> (Cross, updateMatrix bboard Circle loc)
    False -> Nothing

getCellOfLocation :: Location -> GameState -> Either Cell Outcome --needs to return maybe for check cell
getCellOfLocation (bigIndex, miniIndex) (_,bboard)
  | bigIndex < 0 || miniIndex < 0 || bigIndex > 8 || miniIndex > 8 = error "IndexOutOfBound in getCellOfLocation"
  | otherwise =
      case (bboard !! bigIndex) of 
        Game cells -> Left $ cells !! miniIndex
        Winner Nothing        -> Right Tie
        Winner (Just player)  -> Right (Win player)

checkCell :: Location -> GameState -> Bool -- do bounds checking in here to retun false so code doesnt crash
checkCell location gs = 
  case getCellOfLocation location gs of 
          Right _ -> False
          Left (Nothing) -> True
          Left (Just player) -> False

getLegalMoves :: GameState -> [Location] 
getLegalMoves = undefined --zip bigboard with bigboard indexes and zip miniboard with miniboard indexes 

emptyBoard = take 9 $ repeat (Game $ take 9 $ repeat Nothing)
allXBoard = take 9 $ repeat (Game $ take 9 $ repeat (Just Cross))

showGameState :: GameState -> String --BigBoard 
showGameState (turn, bigboard) = unlines ["Current turn: " ++ show turn ++ "\n", showBigBoard bigboard]


showFor =
--Show Player = --or something
 -- show Cross  = " x "
 -- show Circle = " o "

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
