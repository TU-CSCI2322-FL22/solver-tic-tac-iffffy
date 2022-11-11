import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import TysPrim (threadIdPrimTyCon)

main :: IO ()
main = return ()

data Player = Cross | Circle deriving (Show, Eq)
type Cell = Maybe Player
newtype MiniBoard = Game [Cell] deriving (Show, Eq) --Miniboard will be a bunch of cells
type BigBoard = [(Maybe Player, MiniBoard)]         --Bigboard will be a list of tuples that remembers who wins in the Miniboard
type Turn = Player
-- Cursor parking [ | M | J😩 | K 🥵| R | L | ]
-- Cursor cafe [🍊 🥐 🥗 🍰 🥪]
type BigBoardIndex = Int
type MiniBoardIndex = Int
type GameState = (Turn, BigBoard)
data Outcome = Win Player | Tie deriving (Eq) --need to refactor to have either win or tie
type Location = (BigBoardIndex, MiniBoardIndex)

-- list of possible moves for win
possibleWins = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]

-- return the list of MiniBoardIndex (a.k.a cell location in MiniBoard) where the player has filled.
squaresFor :: Player ->  MiniBoard -> [Int]
squaresFor player (Game mb) = [ loc | (loc, piece) <- zip [0..] mb, piece == Just player]

-- determine winner of a miniboard
-- return whether the player win the miniBoard
miniWinner :: Player -> MiniBoard -> Bool
miniWinner player mb = didIWin (squaresFor player mb)

-- return whether the player win (either mini or big board) according to the list of indices (either BigBoardIndices or MiniBoardIndices)
didIWin :: [Int] -> Bool
didIWin indices =
  any (all (`elem` indices)) possibleWins

-- return the list of BigBoardIndices (a.k.a miniBoard location in BigBoard) where the player has won.
winnersFor :: Player -> BigBoard -> [Int]
winnersFor player bboard =
  let (states, _) = unzip bboard
  in [ loc | (loc, piece) <- zip [0..] states, piece == Just player ]
  
-- return the outcome of the game at that game-state
gameStateWinner :: GameState -> Maybe Outcome -- needs to return maybe outcome
gameStateWinner (turn, bigBoard)
  | didIWin (winnersFor turn bigBoard) = Just $ Win turn
  | didIWin (winnersFor (anotherTurn turn) bigBoard) = Just $ Win (anotherTurn turn)
  | cannotWinAtAll Cross bigBoard && cannotWinAtAll Circle bigBoard = Just Tie
  |  otherwise = Nothing


cannotWinAtAll :: Player -> BigBoard -> Bool
-- Determine if the player still have chance to win the Game
cannotWinAtAll p bigBoard =
  -- currently, this return True if there exist no empty cell and both players did not win
  null (getLegalMoves (p,bigBoard)) 
    && not (didIWin (winnersFor Cross bigBoard) && didIWin (winnersFor Circle bigBoard))

-- cannotWinMiniBoard :: Player -> MiniBoard -> Bool
-- -- Determine if the player still have chance to win the mini board
-- cannotWinMiniBoard p (Game miniBoard) =
--   -- currently, this return True if there exist no empty cell and both players did not win
--   not any (==Nothing) miniBoard
--     && not (didIWin (squaresFor Cross miniBoard) && didIWin (squaresFor Circle min a a))
-- cannotWinMiniBoard p (Winner _) = False

{-
updateMatrix m x (r,c) =
  let (Game cellsAtC) = m !! r 
  in take r m ++
    [Game $ take (c-1) cellsAtC ++ [Just x] ++ drop (c + 1) cellsAtC] ++
    drop (r + 1) m
-}
-- updating a miniboard and bigboard
updateMatrix :: BigBoard -> Player -> Location -> BigBoard
updateMatrix bb x (r,c) =
  let (leftBoard,currentBoard:rightBoard) = splitAt r bb
      (currentWinner, Game cellsAtC) = currentBoard
      updatedCells = Game $ take (c-1) cellsAtC  ++ [Just x] ++ drop (c + 1) cellsAtC
  in if miniWinner x updatedCells 
        then leftBoard++[(Just x, updatedCells)]++rightBoard
     else leftBoard++[(Nothing, updatedCells)]++rightBoard
  

 --applies move to gamestate, uses anotherturn
makeMove :: GameState -> Location -> Maybe GameState
makeMove (turn, bboard) loc =
  if checkCell loc (turn, bboard) then Just (anotherTurn turn, updateMatrix bboard Cross loc)
  else Nothing --error "Illegal move"

-- return the complement of current turn, keeps turn order
anotherTurn :: Turn -> Turn 
anotherTurn Cross  = Circle
anotherTurn Circle = Cross


--applies index to cells to enumerate their locations
getCellOfLocation :: Location -> GameState -> Maybe Cell
getCellOfLocation (bigIndex, miniIndex) (_,bboard)
  | bigIndex < 0 || miniIndex < 0 || bigIndex > 8 || miniIndex > 8 = Nothing -- error "IndexOutOfBound in getCellOfLocation"
  | otherwise = let (states, miniboards) = unzip bboard 
                    (Game thatMiniBoard) = (miniboards !! bigIndex) 
                in Just $ thatMiniBoard !! miniIndex

-- checks if cell is empty or not, returns true or false for legal moves
checkCell :: Location -> GameState -> Bool
checkCell location gas =
  let thatCell = getCellOfLocation location gas
  in case thatCell of 
      Nothing            -> False
      Just Nothing       -> True
      Just (Just _)      -> False

 --use check cell to return list of possible legal moves
getLegalMoves :: GameState -> [Location]
getLegalMoves gas = [(x,y) | x <- [0..8], y <- [0..8], x < y, checkCell (x,y) gas]

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
----------------
                      ----- Milestone 2 -----
--simple interface--
-- readGame :: String -> GameState       --Reads the game state from file
-- readGame str
-- str
--   | str == "Cross;_" = (Cross,[])
--   | str == "Circle;_" = (Circle,[])
--   | otherwise = 
--     let stuff = head splitOn ";" str
--         newstuff = tail splitOn ";" str
--     in (stuff,newstuff)
--insert a string with a turn and bigboard
--bigBoard is a list of miniboards, which is a list of cells
--pseudocode:

showGame :: GameState -> String       --Shows the file
showGame = undefined -- showGameState "" 

writeGame :: GameState -> FilePath -> IO () --writes game-state from file and converts to IO
writeGame gameState path = writeFile path $ showGameState gameState ""

loadGame :: FilePath -> IO GameState --
loadGame path = undefined -- let context <- readFile path in context

------
putWinner :: GameState -> IO () --computes and prints winning move
putWinner = undefined


--split whoWillWin into helper functions: critical and gain win
--the problem is being split like this because we need to check two conditions:
--1: the current sign (circle or cross) needs to block their opponent from winning on their next turn
--2: look for the best possible winning conditions 

critical :: GameState -> ([Location],[Location]) --Checks for moves that help guarantee a win for the player 3
critical gas =                      --for some reason gameState is now called "gas", thanks Raven lol
  let (turn, bigBoard) = gas
      (states, miniBoards) = unzip bigBoard
      noWinnerBigIndices = [ x |(x,y) <- zip [0..8] states, isNothing y]
      possibleMoves = [ (x,y) | (x,y) <- getLegalMoves gas, x `elem` noWinnerBigIndices]
      (miniWinMoves, bigWinMoves) =
        foldl (\lst loc -> 
                         let (newStates,_) = unzip (updateMatrix bigBoard turn loc) 
                             miniList = if newStates /= states then loc:fst lst else fst lst
                             bigList = if didIWin [ index | (index,state) <- zip [0..8] newStates, state == Just turn]
                                      then loc:fst lst else fst lst              
                          in (miniList, bigList) 
                               ) ([],[]) possibleMoves
  in (sortCriticalOfMiniBoards gas $ filter (`notElem` bigWinMoves) miniWinMoves,bigWinMoves)
  
sortCriticalOfMiniBoards :: GameState -> [Location] -> [Location]
-- sort the locations according to best places to build win (a.k.a second mark)
sortCriticalOfMiniBoards _ [] = []
sortCriticalOfMiniBoards gas locations = undefined
  
  where goodSecondPlaces :: [Int] -> [Int] -> ([Int],[Int])
  -- return
        goodSecondPlaces enemyIndices myIndices = 
          let remaining = filter (`notElem` enemyIndices ++ myIndices) [0..8]
          in case filter (\x -> (any (`elem` myIndices) x) && not (any (`elem` enemyIndices) x)) possibleWins  of
            []                  -> ([], remaining)
            currentPossibleWins -> undefined--map (\lst -> (head lst, length lst)) $ filter (`elem` remaining) $ groupBy (==) $ concat currentPossibleWins


--call gamestatewinner after we make a move in order to double check
whoWillWin :: GameState -> Outcome --Checks who's the closest to winning
whoWillWin = undefined

bestMove :: GameState -> Player
bestMove = undefined
-- first check critical of enemy (fakeGas = (enemy, bigBoard))
-- if return critical == ([],[])
-- then check critical for me (gas)
-- prioritzie critical locations where I'll win bboard, if bboard loc == [], get loc of miniboard to maximize win.


-- checks the best second location for player on miniboard assuming there are no good first locations
bestSndLocation locs player mb = let sqs = squaresFor player mb 
                                 in [[[loc | sq <- sqs, sq `elem` win] | win <- possibleWins, loc `elem` win] | loc <- locs]