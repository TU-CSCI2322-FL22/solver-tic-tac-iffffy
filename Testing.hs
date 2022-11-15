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

compBoard = (Cross, [(Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle,
                                     Just Cross, Just Circle, Just Cross]),
                     (Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]), 
                     (Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]),
                     (Cross, Game [Just Cross, Just Cross, Just Cross,
                                     Just Circle, Just Cross, Just Circle,
                                     Nothing, Just Cross, Just Circle]),
                     (Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle]),
                     (Cross, Game [Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle,
                                     Just Cross, Just Cross, Just Circle])])

noneBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 Nothing)))
circleBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 (Just Circle))))
crossBoard = (Circle, replicate 9 (Nothing, Game (replicate 9 (Just Cross))))
--putStrLn (showGameState compBoard " ")
--putStrLn (showGameState noneBoard " ")    
    
main = do
        putStrLn (showGameState circleBoard " ") 
        if (gameStateWinner circleBoard) == Just (Win Cross) then putStrLn "Cross Won"
        else if (gameStateWinner circleBoard) == Just (Win Circle) then putStrLn "Circle Won"
        else putStrLn (showOutcome(gameStateWinner circleBoard))