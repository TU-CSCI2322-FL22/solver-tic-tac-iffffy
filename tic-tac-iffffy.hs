type Cell = Maybe Player
data Player = Cross | Circle deriving (Show, Eq)
type Board = [[ Cell ]]
type MiniBoard = [ Maybe Player ]
type BigBoard = [MiniBoard] 

type MBWinner = (Maybe Player, MiniBoard)
type BBWinner =  (Maybe Player, [MBWinner]) -- (Maybe Player, BigBoard)

Nothing -> let newBoards =  updateWinners x board
           in if thereIsWinner 
              then celebrate
              else waitForNewMove
Just x     -> error “Overwriting move”

possibleWins = [[0,1,2],[3,4,5],[6,7,8],
                [0,3,6],[1,4,7],[2,5,8],
                [0,4,8],[2,4,6]]

type Location = (BigBoardIndex :: Int, MiniBoardIndex :: Int)

updateBoardWinners (bigLoc, smallLoc) board =
  let miniboard = board !! bigLoc


