module Testing where
import Solver
import RnD
import Test.QuickCheck
import Game
import Test.Hspec
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Typeable
         
xWinBoard = (Cross, createBigBoard [Game [Just Cross, Nothing, Nothing,
                                     Nothing, Just Cross, Nothing,
                                     Just Cross, Just Circle, Just Cross],
                                  Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle], 
                                  Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Circle,
                                     Just Cross, Just Cross, Just Circle],
                     Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle],
                     Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Circle, Just Circle, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Cross]])

oWinBoard = (Circle, createBigBoard [Game [Just Circle, Nothing, Nothing,
                                     Nothing, Just Circle, Nothing,
                                     Just Cross, Just Circle, Just Circle],
                     Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle], 
                     Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Cross,
                                     Just Cross, Just Cross, Just Circle],
                     Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Cross, Just Cross,
                                     Nothing, Just Cross, Just Circle],
                     Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Circle, Just Circle, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                     Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Circle,
                                     Nothing, Nothing, Just Cross]])

oLikelyWin1Step = (Circle, createBigBoard [Game [Just Circle, Nothing, Nothing,
                                     Nothing, Just Circle, Nothing,
                                     Just Cross, Just Circle, Nothing],
                     Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Cross], 
                     Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Circle,
                                     Just Cross, Just Cross, Just Circle],
                     Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Cross, Just Cross,
                                     Nothing, Just Cross, Just Circle],
                     Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Circle, Just Circle,
                                     Nothing, Just Cross, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Circle, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                     Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Circle,
                                     Nothing, Nothing, Just Circle]])

xLikelyWin2Step = (Circle, createBigBoard [Game [Just Cross, Nothing, Nothing,
                                     Nothing, Just Cross, Nothing,
                                     Just Cross, Just Circle, Just Cross],
                     Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle], 
                     Game [Just Circle, Just Cross, Nothing,
                                     Just Cross, Just Circle, Just Circle,
                                     Just Cross, Just Cross, Just Circle],
                     Game [Just Cross, Just Cross, Nothing,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle],
                     Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Cross],
                     Game [Just Cross, Just Cross, Nothing,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Circle, Just Circle, Just Circle],
                     Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing]])

tieBoard = (Cross, createBigBoard [Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle], 
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle],
                    Game [Just Cross, Just Cross, Just Circle,
                                     Just Circle, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle]])

emptyBoard = (Circle, createBigBoard (replicate 9 (Game $ replicate 9 Nothing)))
noneBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 Nothing)))
circleBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 (Just Circle))))
crossBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 (Just Cross))))
--putStrLn (showGameState compBoard " ")
--putStrLn (showGameState noneBoard " ")  
--putStrLn (showGameState xWinBoard " ")    
--putStrLn (showGameState noneBoard " ")   
--putStrLn (showGameState oLikelyWin1Step " ")
--putStrLn (showGameState xLikelyWin2Step " ") 

--putStrLn (showGameState xLikelyWin2Step " ")   
--putStrLn (show $ fst $ unzip $ snd xLikelyWin2Step)
{-
let field = readGame "xxxxxxxxx\nxxxxxxxxx\nxxxxxxxxx\nxxxxxxxxx\nxxxxxxxxx\nxxxxxxxxx\nxxxxxxxxx\nxxxxxxxxx\nxxxxxxxxx\no"
    field1 = readGame "xx_xxx_ox\nxxx__oxxx\nooooooooo\nxxoox__xx\nx__xxxoxx\nxooxxx_xx\nxooxxxxxx\nxxxxxxxxx\nxxxxxxxxx\nx"
    field2 = readGame "xxx_x__xx\nx_xxxx__x\n___xxxxxx\nxxx___xxx\nxxxxxx___\n_xxxxxxx_\nxxxxxxxxx\nxoxxxxxox\nxoxoxox__\no"
in do 
            putStrLn (showGameState field " ")
            putStrLn (showGameState field1 " ")
            putStrLn (showGameState field2 " ") 
-}
main :: IO()    
main = do x <- loadGame "board.txt"
          putStrLn (showGameState x "") 
        
    {-
    hspec $ do
        describe "Checking Winners" $ do
            it "board of circles" $ do
                gameStateWinner circleBoard `shouldBe` Win Circle
            it "board of crosses" $ do
                gameStateWinner crossBoard `shouldBe` Win Cross
            it "board of mixed X Winner" $ do
                gameStateWinner xWinBoard `shouldBe` Win Cross
            it "Tie Board Winner" $ do
                gameStateWinner tieBoard `shouldBe` Tie
            it "bestMove" $ do
                bestMove oLikelyWin1Step `shouldBe` Just (0,8)
            it "bestMove 2X" $ do
                bestMove xLikelyWin2Step `shouldBe` Just (0,8)
            it "who will win o 1 step" $ do
                whoWillWin oLikelyWin1Step `shouldBe` Win Circle
            it "who will win x 2 steps" $ do
                whoWillWin xLikelyWin2Step `shouldBe` Win Cross
            it "who will win empty board" $ do
                whoWillWin emptyBoard `shouldBe` Tie
            {-
            it "board of mixed O Winner" $ do
                gameStateWinner oWinBoard `shouldBe` Just (Win Circle)
            it "board of mixed O Winner" $ do
                gameStateWinner oWinBoard `shouldBe` Just (Win Circle)
            it "board of mixed O Winner" $ do
                gameStateWinner tieBoard `shouldBe` Just Tie
            
            it "who will win o 1 step" $ do
                whoWillWin oLikelyWin1Step `shouldBe` Just (Win Circle)
            it "who will win x 2 steps" $ do
                whoWillWin xLikelyWin2Step `shouldBe` Just (Win Cross)
            
            it "readboard X" $ do
                readGame readGame "XXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nO\n8" `shouldBe` xWinBoard
            it "readboard O" $ do
                readGame readGame "XXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nXXXXXXXXX\nO\n8" `shouldBe` oWinBoard
        -}
    -}