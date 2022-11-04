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
type BigBoardIndex = Int
type MiniBoardIndex = Int
type GameState = (Turn, BigBoard)
type Outcome = (Player, MiniBoard)
type Location = (BigBoardIndex, MiniBoardIndex)

possibleWins = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]

gameStateWinner :: GameState -> Player
gameStateWinner = undefined

makeMove :: GameState -> GameState
makeMove = undefined

getCellOfLocation :: Location -> Cell --checkCell helper, use in legal moves 
getCellOfLocation loc = undefined

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

showGameState :: GameState -> BigBoard 
showGameState = undefined

