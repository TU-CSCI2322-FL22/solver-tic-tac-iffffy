module Game where
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Foldable
--import TysPrim (threadIdPrimTyCon)                      --WE IMPORTED IT BUT THEY'RE GIVING ERRORS FOR SOME REASON
--import GhcPlugins (boxingDataCon_maybe, xFlags)         --ALSO, I DON'T KNOW WHY WE HAVE THESE TWO IMPORTS
-- main :: IO ()
-- main = return ()

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
data Outcome = Win Player | Tie deriving (Show, Eq)
type Location = (BigBoardIndex, MiniBoardIndex)


{-
-- list of possible moves for win
possibleWins = [[0,1,2],[3,4,5],[6,7,8],
               [0,3,6],[1,4,7],[2,5,8],
               [0,4,8],[2,4,6]]

createBigBoard :: [MiniBoard] -> BigBoard
createBigBoard lst = 
  [ let didCrossWin = miniWinner Cross miniBoard
        didCircleWin = miniWinner Circle miniBoard
    in if didCrossWin then (Just Cross, miniBoard) 
       else if didCircleWin then (Just Circle, miniBoard) 
            else (Nothing, miniBoard) | miniBoard <- lst]
  
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
gameStateWinner :: GameState -> Outcome -- needs to return maybe outcome
gameStateWinner (turn, bigBoard)
  | didIWin (winnersFor turn bigBoard) = Win turn
  | didIWin (winnersFor (anotherTurn turn) bigBoard) = Win (anotherTurn turn)
  -- | cannotWinAtAll Cross bigBoard && cannotWinAtAll Circle bigBoard = Tie
  |  otherwise = Tie


-- cannotWinAtAll :: Player -> BigBoard -> Bool
-- -- Determine if the player still have chance to win the Game
-- cannotWinAtAll p bigBoard =
--   -- currently, this return True if there exist no empty cell and both players did not win
--   null (getLegalMoves (p,bigBoard))
--     && not (didIWin (winnersFor Cross bigBoard) && didIWin (winnersFor Circle bigBoard))

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
getLegalMoves gas = [(x,y) | x <- [0..8], y <- [0..8], checkCell (x,y) gas]

          --- FOR VISUALIZATIONS ---

-- emptyBoard = replicate 9 (Game $ replicate 9 Nothing)
-- allXBoard = replicate 9 (Game $ replicate 9 (Just Cross))

--BigBoard, user interface showing
showGameState :: GameState -> String -> String
showGameState (turn, bboard) message =
  unlines ["Current turn: " ++ show turn, "", showBigBoard bboard, "", message, ""]

 --cell char show
showCell :: Cell -> String
showCell Nothing = "   "
showCell (Just Cross) = " x "
showCell (Just Circle) = " o "

--- ugly cells :(( need for read functions
uglyCell :: Cell -> String 
uglyCell Nothing = " "
uglyCell (Just Cross) = "x"
uglyCell (Just Circle) = "o"

--uglyBoard :: BigBoard -> String
--uglyBoard bigBoard = chunksOf 3 uglyCell
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
          intercalate "\n" $ map (intercalate "||") $ transpose $ map (splitOn "\n" . showMiniBoard "\n") panel
----------------
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
sortCriticalOfMiniBoards (turn,bigBoard) locations =
  let enemyIndices = winnersFor (anotherTurn turn) bigBoard
      myIndices = winnersFor turn bigBoard
      orderOfBigIndices = goodSecondPlaces enemyIndices myIndices
  in sortWithOrder orderOfBigIndices locations
  where sortWithOrder :: [BigBoardIndex] -> [Location] -> [Location]
        sortWithOrder lst locs = concat [ filter (\(x,y) -> x == i) locs | i <- lst ]

goodSecondPlaces :: [Int] -> [Int] -> [Int]
goodSecondPlaces enemyIndices myIndices =
  -- In case of no location leads to direct win, this generate list of locations 
  -- where the first one(s) is the best move according to number of winning path it can open
  let remaining = filter (`notElem` enemyIndices ++ myIndices) [0..8]
      currentPossibleWins =
        case filter (\x -> any (`elem` myIndices) x && not (any (`elem` enemyIndices) x)) possibleWins  of
            [] -> possibleWins
            x -> x
      good = filter (`elem` remaining) $ map fst (last $ groupBy (\(_,x) (_,y) -> x == y) $ sortOn snd $ map (\lst -> (head lst, length lst)) $ groupBy (==) $ sort $ concat currentPossibleWins)
  in traceShow (good,remaining) $ good ++ filter (`notElem` good) remaining


--call gamestatewinner after we make a move in order to double check
whoWillWin :: GameState -> Outcome --Checks who's the closest to winning
whoWillWin gas = 
  let final_gas = aux gas 81
  in traceShow (fst $ unzip $ snd final_gas) $ gameStateWinner final_gas

  where aux :: GameState -> Int -> GameState
        aux gas 0 = gas
        aux gas iter = 
          case gameStateWinner gas of 
            Win x -> gas
            Tie   -> let move = bestMove gas
                     in if move == Nothing then gas
                        else case makeMove gas (fromJust move) of
                                Nothing -> error "Serious error, bestMove generated illegal move"
                                Just x  -> aux x (iter-1)
                    

-- Just brainstorming: measure is by who own more winning paths on BigBoard? does win potential on miniBoards matters?
                    -- if equals?
                    -- if both == 0?

bestMove :: GameState -> Maybe Location
bestMove (turn, bigBoard)=
  let enemy = anotherTurn turn
      (iDontWin,iWin) = critical (turn,bigBoard)
  in case iWin of
    x:xs  -> Just x
    [] -> let (enemydontWin,enemyWin) = critical (enemy,bigBoard)
          in case enemyWin of
            [x]   -> Just x
            x:xs  -> Just x -- can I surrender :) enemy has more than 1 way to win bigBoard
            []    -> if iDontWin == [] then Nothing else Just (head iDontWin)

-- First check critical for me to see if I can win the whole game by 1 step
-- if I cannot win now, then
-- first check critical of enemy (fakeGas = (enemy, bigBoard))
-- if enemy also cannot win now
-- check get loc of miniboard to maximize win.



-- checks the best second location for player on miniboard assuming there are no good first locations
bestSndLocation locs player mb = let sqs = squaresFor player mb
                                 in [[[loc | sq <- sqs, sq `elem` win] | win <- possibleWins, loc `elem` win] | loc <- locs]
-}