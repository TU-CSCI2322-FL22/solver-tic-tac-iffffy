module Main where
import           Data.List
import           Data.Char
import           Data.Maybe
import           Debug.Trace
import           System.Random (mkStdGen, randomR) -- use "cabal install --lib random" if you don't have this package
import           Game
import           RnD
import           Solver

import           System.Console.GetOpt
import           System.Environment
import           System.Exit

import Control.Monad (when)
import Data.ByteString (hPutStr)

defaultFile :: String
defaultFile = "board.txt"

data Options = Options {
  optHelp     :: Bool,
  optWinner   :: Bool,
  optDepth    :: Int,
  optMove     :: String,
  optVerbose  :: Bool,
  optInterAct :: Bool,
  fname        :: String
 } deriving (Show)

defaultOptions :: Options
defaultOptions = Options  
  {
  optHelp     = False,
  optWinner   = False,
  optDepth    = -1,
  optMove     = "",
  optVerbose  = False,
  optInterAct = False,
  fname        = defaultFile
  }

options :: [OptDescr (Options -> IO Options)]
options = [
  Option ['h'] ["help"]        (NoArg  (\opt      -> return opt { optHelp = True })) "Print a help message and exit.",
  Option ['w'] ["winner"]      (NoArg  (\opt      -> return opt { optWinner = True })) "Print out the best move, using an exhaustive search (no cut-off depth).",
  Option ['d'] ["depth"]       (ReqArg (\arg opt  -> return opt { optDepth = read arg }) "Int" ) "Use <num> as a cutoff depth, instead of your default.",
  Option ['m'] ["move"]        (ReqArg (\arg opt  -> return opt { optMove= arg }) "[0..8][a..z]") "Make <move> and print out the resulting board, in the input format, to stdout. The move should be 1-indexed. If a move requires multiple values, the move will be a tuple of numbers separated by a comma with no space.",
  Option ['v'] ["verbose"]     (NoArg  (\opt      -> return opt { optVerbose = True })) "Output both the move and a description of how good it is: win, lose, tie, or a rating.",
  Option ['i'] ["interactive"] (NoArg  (\opt      -> return opt { optInterAct = True })) "Start a new game and play against the computer. Make sure this is compatible with the -d flag.",
  Option ['f'] ["file"]        (ReqArg (\arg opt  -> return opt { fname = arg }) "FILE") "Input gamestate file."
  ]


main :: IO ()
main = do
  allArgs <- getArgs
  
  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options allArgs

  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions

  let Options   {
    optHelp     = help,
    optWinner   = winner,
    optDepth    = depth,
    optMove     = move,
    optVerbose  = verbose,
    optInterAct = interactive,
    fname        = fname
    } = opts

  let searchDepth = if depth > 0 then depth else 81 -- 81 is max iteration.

  when help (do printHelp; exitSuccess)
  when winner (printWinner fname)
  
  when (move /= "") (printMakeMove fname) 
  when verbose (do printEvalMove searchDepth fname;)
  when interactive (startGame fname)
  
  

printHelp :: IO ()
printHelp = do
  putStrLn "Here is HELP"

printWinner :: String -> IO ()
printWinner fname = putStrLn "Here is Winner"

printMakeMove :: String ->  IO ()
printMakeMove fname = putStrLn "Here is Best Move"

printEvalMove :: Int -> String -> IO ()
printEvalMove depth fname = putStrLn "Here is Evaluation"

startGame :: String -> IO ()
startGame fname = putStrLn "Here is Start game"