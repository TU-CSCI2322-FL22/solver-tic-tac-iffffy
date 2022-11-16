module Testing where
import Test.QuickCheck
import Game
import Test.Hspec
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Typeable
         
xWinBoard = (Cross, [(Just Cross, Game [Just Cross, Nothing, Nothing,
                                     Nothing, Just Cross, Nothing,
                                     Just Cross, Just Circle, Just Cross]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]), 
                     (Just Circle, Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Circle, Just Circle, Just Circle]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle]),
                     (Nothing, Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Cross])])

oWinBoard = (Circle, [(Just Circle, Game [Just Circle, Nothing, Nothing,
                                     Nothing, Just Circle, Nothing,
                                     Just Cross, Just Circle, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]), 
                     (Just Circle, Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Cross,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Cross, Just Cross,
                                     Nothing, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Circle, Just Circle, Just Circle]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle]),
                     (Nothing, Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Circle,
                                     Nothing, Nothing, Just Cross])])

oLikelyWin1Step = (Circle, [(Just Circle, Game [Just Circle, Nothing, Nothing,
                                     Nothing, Just Circle, Nothing,
                                     Just Cross, Just Circle, Nothing]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Cross]), 
                     (Just Circle, Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Cross, Just Cross,
                                     Nothing, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Circle, Just Circle,
                                     Nothing, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Circle, Just Circle]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle]),
                     (Nothing, Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Circle, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Circle,
                                     Nothing, Nothing, Just Circle])])

xLikelyWin2Step = (Cross, [(Just Cross, Game [Just Cross, Nothing, Nothing,
                                     Nothing, Just Cross, Nothing,
                                     Just Cross, Just Circle, Just Cross]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]), 
                     (Just Circle, Game [Just Circle, Just Cross, Nothing,
                                     Just Cross, Just Circle, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Nothing,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Cross]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Cross,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Circle, Just Circle, Just Circle]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle]),
                     (Nothing, Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing])])

tieBoard = (Cross, [(Just Cross, Game [Just Cross, Nothing, Nothing,
                                     Nothing, Just Cross, Nothing,
                                     Just Cross, Just Circle, Just Cross]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]), 
                     (Just Circle, Game [Just Circle, Just Cross, Nothing,
                                     Just Cross, Just Circle, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Nothing,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Cross]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Cross,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Circle, Just Circle, Just Circle]),
                     (Just Circle, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Just Circle]),
                     (Nothing, Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Nothing, Nothing, Nothing])])

noneBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 Nothing)))
circleBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 (Just Circle))))
crossBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 (Just Cross))))
--putStrLn (showGameState compBoard " ")
--putStrLn (showGameState noneBoard " ")  
--putStrLn (showGameState xWinBoard " ")    
--putStrLn (showGameState noneBoard " ")   
--putStrLn (showGameState oLikelyWin1Step " ")
--putStrLn (showGameState xLikelyWin2Step " ") 
main :: IO()    
main = hspec $ do
        describe "Checking Winners" $ do
            it "board of circles" $ do
                gameStateWinner circleBoard `shouldBe` Just (Win Circle)
            it "board of crosses" $ do
                gameStateWinner crossBoard `shouldBe` Just (Win Cross)
            it "board of mixed X Winner" $ do
                gameStateWinner xWinBoard `shouldBe` Just (Win Cross)
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