module Solver where 
import Game 
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Foldable
import Data.Ratio

import Data.Either
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

miniTie :: MiniBoard -> Bool
miniTie (Game mb) = (length mb) == (length (catMaybes mb))

isTie :: BigBoard -> Bool
isTie bb = let (_, brds) = unzip bb
               bw = [miniTie mb | mb <- brds]  
           in all (==True) bw

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
  | didIWin (winnersFor turn bigBoard) = Just (Win turn)
  | didIWin (winnersFor (anotherTurn turn) bigBoard) = Just (Win (anotherTurn turn))
  | isTie bigBoard = Just Tie
  | otherwise = Nothing

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
makeMove gas@(turn, bboard) loc =
  case  gameStateWinner gas of 
    Nothing -> 
        if checkCell loc (turn, bboard)
          then Just (anotherTurn turn, updateMatrix bboard turn loc)
        else Nothing --error "Illegal move"
    Just (Win x) -> Just (x, updateMatrix bboard turn loc)
    Just Tie -> Nothing

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
                    state = (states !! bigIndex)
                in if state == Nothing then Just $ thatMiniBoard !! miniIndex else Nothing

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


--call gamestatewinner after we make a move in order to double check
whoWillWin :: GameState -> Outcome --Checks who's the closest to winning
whoWillWin gas@(turn,bboard) =
  case gameStateWinner gas of
    Just x  -> x
    Nothing ->
      case getLegalMoves gas of
        []         -> error "Should have legal moves when game is on-going"
        legalMoves ->
          let nextGases = mapMaybe (makeMove gas) legalMoves
              outcomes = map whoWillWin nextGases
          in bestOutcomeFor turn outcomes

bestOutcomeFor :: Turn -> [Outcome] -> Outcome
bestOutcomeFor turn outcomes = 
  if Win turn `elem` outcomes then Win turn
  else if Tie `elem` outcomes then Tie
       else Win (anotherTurn turn)


bestMove :: GameState -> Maybe Location
bestMove gas@(turn, bigBoard) =
  case gameStateWinner gas of
    Just _ -> Nothing -- game has completed
    Nothing ->
      case getLegalMoves gas of
        [] -> error "Should not be the case for on-going game"
        moves -> 
          let outcomes = map whoWillWin $ mapMaybe (makeMove gas) moves
          in if length outcomes /= length moves then error "Should not happen at all"
             else bestMoveFor turn outcomes moves

bestMoveFor :: Turn -> [Outcome] -> [Location] -> Maybe Location
bestMoveFor turn outcomes locs = 
  let locNout = zip locs outcomes 
  in if Win turn `elem` outcomes then
        Just $ fst $ head $ filter (\(loc,out) -> out == Win turn ) locNout
      else if Tie `elem` outcomes then 
          Just $ fst $ head $ filter (\(loc,out) -> out == Tie ) locNout
        else Just $ head locs

-- evaluation of game
scoreGame :: GameState -> Int 
scoreGame gas@(t,bboard) =
  case gameStateWinner gas of
    Nothing -> 
      let (bboardIndx1,bboardIndx2) =(winnersFor Cross bboard,winnersFor Circle bboard)
          miniIndx1 = map (\(_,mb) -> squaresFor Cross mb) bboard
          miniIndx2 = map (\(_,mb) -> squaresFor Circle mb) bboard
          scoreP1 = sum $ map (\(a,b) -> scoreIndices a b) $ zip (bboardIndx1:miniIndx1) (bboardIndx2:miniIndx2)
          scoreP2 = sum $ map (\(a,b) -> scoreIndices b a) $ zip (bboardIndx1:miniIndx1) (bboardIndx2:miniIndx2)
      in scoreP1 - scoreP2
    Just x -> scoreOutcome x

scoreIndices :: [Int] -> [Int] -> Int
scoreIndices p1 p2 = 
  let scoreP1 = fst $ maximum $ [ (length [ x | x <- p, x `elem` p1],length [ x | x <- p, x `elem` p2]) | p <- possibleWins ]
      scoreP2 = fst $ maximum $ [ (length [ x | x <- p, x `elem` p2],length [ x | x <- p, x `elem` p1]) | p <- possibleWins ]
  in scoreP1 - scoreP2

scoreOutcome :: Outcome -> Int
scoreOutcome (Win Cross) = 2000
scoreOutcome (Win Circle) = -2000
scoreOutcome Tie = 0

bestScoreFor  :: Ord a => Turn -> [a] -> a
bestScoreFor Cross scs = maximum scs
bestScoreFor Circle scs = minimum scs
  
--call gamestatewinner after we make a move in order to double check
whoMightWin :: GameState -> Int ->  Int
whoMightWin gas@(turn,bboard) 0 =
  case gameStateWinner gas of
    Nothing ->  scoreGame gas
    Just x  -> scoreOutcome x


whoMightWin gas@(turn,bboard) depth =
  case gameStateWinner gas of
    Just x  -> scoreOutcome x
    Nothing ->
      case getLegalMoves gas of
        []         -> error "Should have legal moves when game is on-going"
        legalMoves ->
          let nextGases = mapMaybe (makeMove gas) legalMoves
              scoreOfOutcomes = map (\g -> whoMightWin g (depth-1)) nextGases
              maxScore = bestScoreFor turn scoreOfOutcomes
          in maxScore
             

bestMove2 :: GameState -> Int -> Maybe Location
bestMove2 gas@(turn, bigBoard) depth = 
  case gameStateWinner gas of
    Just _ -> Nothing -- game has completed
    Nothing ->
      case getLegalMoves gas of
        [] -> error "Should not be the case for on-going game"
        moves -> 
          let scores = map (`whoMightWin` depth) $ mapMaybe (makeMove gas) moves
          in if length scores /= length moves then error "Should not happen at all"
             else Just $ fst $ bestScoreFor turn $ zip moves scores
              