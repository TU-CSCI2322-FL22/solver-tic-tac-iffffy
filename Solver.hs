module Solver where 
import Game 
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Foldable


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
      updatedCells = Game $ take c cellsAtC  ++ [Just x] ++ drop (c+1) cellsAtC
  in if miniWinner x updatedCells
        then leftBoard++[(Just x, updatedCells)]++rightBoard
     else leftBoard++[(Nothing, updatedCells)]++rightBoard


 --applies move to gamestate, uses anotherturn
makeMove :: GameState -> Location -> Maybe GameState
makeMove (turn, bboard) loc =
  if checkCell loc (turn, bboard) then Just (anotherTurn turn, updateMatrix bboard turn loc)
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
critical gas@(turn, bigBoard) =                      --for some reason gameState is now called "gas", thanks Raven lol
  let (states, miniBoards) = unzip bigBoard
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

scoreGame :: GameState -> (Outcome,Int)
scoreGame (t,bboard) = 
  let crossScore  = length a + 10 * length b where (a,b) = critical (Cross, bboard) 
      circleScore = length a + 10 * length b where (a,b) = critical (Circle, bboard) 
  in (gameStateWinner (t,bboard), crossScore - circleScore)


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
  in good ++ filter (`notElem` good) remaining


--call gamestatewinner after we make a move in order to double check
whoWillWin :: GameState -> Outcome --Checks who's the closest to winning
whoWillWin gas@(turn,bboard) =
  case gameStateWinner gas of
    Win x -> Win x 
    Tie   -> 
      case getLegalMoves gas of
        []         -> Tie
        legalMoves -> 
          let nextGases = mapMaybe (makeMove gas) legalMoves
              outcomes = map gameStateWinner nextGases
          in 
            if Win turn `elem` outcomes then Win turn
            else if Tie `elem` outcomes then 
              let heh = map whoWillWin $ filter (\g -> gameStateWinner g == Tie) nextGases 
              in if Win turn `elem` heh then Win turn
                 else if Tie `elem` heh then Tie
                 else Win (anotherTurn turn)
              else Win (anotherTurn turn)

depthSearchWin :: GameState -> Int -> (Maybe [Location],Outcome) --Checks who's the closest to winning within number of depth 
depthSearchWin gs depth = 
  case gameStateWinner gs of
  Win x -> (Nothing, Win x)
  Tie   -> 
    let (mbLocs, outcome) = aux gs depth []
    in case mbLocs of
      Nothing -> (Nothing, outcome)
      Just [] -> (Nothing, outcome)
      Just lst -> (Just lst, outcome)
  
  where aux :: GameState -> Int -> [[Location]] -> (Maybe [Location],Outcome)
        aux gas 0 []    = (Nothing,gameStateWinner gas)
        aux gas 0 (x:_) = (Just x,gameStateWinner gas)
        aux gas@(turn,bboard) d locs =
          case getLegalMoves gas of
            []         -> (Nothing,gameStateWinner gas)
            legalMoves -> 
              let nextGases = mapMaybe (makeMove gas) legalMoves
                  outcomes = map gameStateWinner nextGases
                  daZip = zip legalMoves outcomes
              in 
                if Win turn `elem` outcomes then 
                  let updatedLocs = locs ++ [[ a | (a,b) <- daZip, b == Win turn]]
                  in case updatedLocs of
                    []  -> (Nothing, Win turn)
                    x:_ -> (Just x, Win turn)
                else if Tie `elem` outcomes then 
                  let updatedLocs = locs ++ [[ a | (a,b) <- daZip, b == Tie]]
                      hehuh = map (\x -> aux x (d-1) updatedLocs) $ filter (\g -> gameStateWinner g == Tie) nextGases 
                      heh = map snd hehuh
                  in if Win turn `elem` heh then
                    let updatedLocs2 = locs ++ [[ a | (a,b) <- daZip, b == Win turn]]
                    in case updatedLocs2 of
                      []  -> (Nothing, Win turn)
                      x:_ -> (Just x, Win turn)
                    else if Tie `elem` heh then 
                      let updatedLocs2 = locs ++ [[ a | (a,b) <- daZip, b == Tie]]
                      in case updatedLocs2 of
                        []  -> (Nothing, Tie)
                        x:_ -> (Just x, Tie)
                    else 
                      let updatedLocs2 = locs ++ [[ a | (a,b) <- daZip, b == Win (anotherTurn turn)]]
                      in case updatedLocs2 of
                        []  -> (Nothing, Win (anotherTurn turn))
                        x:_ -> (Just x, Win (anotherTurn turn))
                  else let updatedLocs2 = locs ++ [[ a | (a,b) <- daZip, b == Win (anotherTurn turn)]]
                    in case updatedLocs2 of
                      []  -> (Nothing, Win (anotherTurn turn))
                      x:_ -> (Just x, Win (anotherTurn turn))


bestMove :: GameState -> Int -> Maybe Location
bestMove gas@(turn, bigBoard) depth =
  let (allLocs, outcome) = depthSearchWin gas depth
  in case allLocs of 
      Nothing -> Nothing
      Just [] -> error "Shouldn't be here as depthSearchWin already eliminate this case"
      Just (x:_) -> Just x

  -- let enemy = anotherTurn turn
  --     (iDontWin,iWin) = critical (turn,bigBoard)
  -- in case iWin of
  --   x:xs  -> Just x
  --   [] -> let (enemydontWin,enemyWin) = critical (enemy,bigBoard)
  --         in case enemyWin of
  --           [x]   -> Just x
  --           x:xs  -> Just x -- can I surrender :) enemy has more than 1 way to win bigBoard
  --           []    -> if iDontWin == [] then Nothing else Just (head iDontWin)

-- First check critical for me to see if I can win the whole game by 1 step
-- if I cannot win now, then
-- first check critical of enemy (fakeGas = (enemy, bigBoard))
-- if enemy also cannot win now
-- check get loc of miniboard to maximize win.



-- checks the best second location for player on miniboard assuming there are no good first locations
bestSndLocation locs player mb = let sqs = squaresFor player mb
                                 in [[[loc | sq <- sqs, sq `elem` win] | win <- possibleWins, loc `elem` win] | loc <- locs]