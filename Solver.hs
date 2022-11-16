module Solver where 
import Game 

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
whoWillWin = undefined
  where aux :: GameState -> Int -> gameState
        aux gas 0 = gas
        aux (turn,bb) iter = 
          cas

-- Just brainstorming: measure is by who own more winning paths on BigBoard? does win potential on miniBoards matters?
                    -- if equals?
                    -- if both == 0?

bestMove :: GameState -> Location
bestMove (turn, bigBoard)=
  let enemy = anotherTurn turn
      (iDontWin,iWin) = critical (turn,bigBoard)
  in case iWin of
    x:xs  -> x
    [] -> let (enemydontWin,enemyWin) = critical (enemy,bigBoard)
          in case enemyWin of
            [x]   -> x
            x:xs  -> x -- can I surrender :) enemy has more than 1 way to win bigBoard
            []    -> head iDontWin

-- First check critical for me to see if I can win the whole game by 1 step
-- if I cannot win now, then
-- first check critical of enemy (fakeGas = (enemy, bigBoard))
-- if enemy also cannot win now
-- check get loc of miniboard to maximize win.



-- checks the best second location for player on miniboard assuming there are no good first locations
bestSndLocation locs player mb = let sqs = squaresFor player mb
                                 in [[[loc | sq <- sqs, sq `elem` win] | win <- possibleWins, loc `elem` win] | loc <- locs]