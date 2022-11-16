module Testing where
import Test.QuickCheck
import Game
import Test.Hspec
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.Typeable
instance Show (a -> b) where
         show a= "funcion"
         
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
                                     Just Cross, Just Circle, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Just Circle, Game [Just Circle, Just Circle, Just Circle,
                                     Just Circle, Just Cross, Just Circle,
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
                                     Nothing, Nothing, Just Circle])])

noneBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 Nothing)))
circleBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 (Just Circle))))
crossBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 (Just Cross))))
--putStrLn (showGameState compBoard " ")
--putStrLn (showGameState noneBoard " ")  
--putStrLn (showGameState xWinBoard " ")    
--putStrLn (showGameState noneBoard " ")    
main :: IO()    
main = hspec $ do
        describe "Checking Winners" $ do -- SHOW FUNCTION TESTING
            it "board of circles" $ do
                gameStateWinner circleBoard `shouldBe` Just (Win Circle)
            it "board of crosses" $ do
                gameStateWinner crossBoard `shouldBe` Just (Win Cross)
            it "board of mixed X Winner" $ do
                gameStateWinner xWinBoard `shouldBe` Just (Win Cross)
            it "board of mixed O Winner" $ do
                gameStateWinner oWinBoard `shouldBe` Just (Win Circle)
