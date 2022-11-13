module Testing where
import Game
import Test.Hspec
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace
main :: IO ()

main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")
