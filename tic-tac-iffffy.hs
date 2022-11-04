main :: IO ()
main = return ()

data Player = Cross | Circle deriving (Show, Eq)
type Cell = Maybe Player
data MiniBoard = Game [Cell] | Winner (Maybe Player)
type BigBoard = [MiniBoard] 
type Turn = Player
type GameState = (Turn, BigBoard)
type Outcome = (Player, MiniBoard)

type MBWinner = (Maybe Player, MiniBoard)
type BBWinner =  (Maybe Player, [MBWinner])
type Location = (Int, Int)

gameStateWinner :: GameState -> Player
gameStateWinner = undefined

makeMove :: GameState -> GameState
makeMove = undefined

getLegalMoves :: GameState -> [Location]
getLegalMoves = undefined

showGameState :: GameState -> Nothing
showGameState = undefined

{-
checkCell :: Location -> ???
checkCell location = 
  case cell of Nothing -> let newBoards = updateWinners x board
                          in if thereIsWinner 
                            then celebrate
                            else waitForNewMove
               Just x  -> error "Overwriting move"
  where 
    cell = getCellOfLocation location
    getCellOfLocation :: Location -> Cell
    getCellOfLocation loc = undefined

possibleWins = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]


updateWinners :: ??? -> Board -> Board
updateBoardWinners (bigLoc, smallLoc) board =
  let miniboard = board !! bigLoc
-}