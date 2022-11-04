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
data Outcome = Win Player | Ongoing BigBoard | Tie deriving (Show, Eq)
type Location = (BigBoardIndex, MiniBoardIndex)

possibleWins = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]

gameStateWinner :: GameState -> Outcome
gameStateWinner = undefined

makeMove :: GameState -> GameState
makeMove = undefined

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

showGameState :: GameState -> String --BigBoard 
showGameState = undefined
