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
