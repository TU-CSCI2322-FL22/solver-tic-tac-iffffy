module Main where
import           Data.List

import           Data.List.Split (splitOn)

import           Data.Char
import           Data.Maybe
import           Debug.Trace
-- import           System.Random (mkStdGen, randomR) -- use "cabal install --lib random" if you don't have this package
import           Game
import           RnD
import           Solver

import           System.Console.GetOpt
import           System.Environment
import           System.Exit

import           Control.Monad (when)
import System.Posix.Internals (puts)



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
  putStrLn "Welcome to Utimate tic-tac-toe"
  putStrLn $ intercalate "\n\t" ["Authors: ","Matvei","Kenneth","Hose","Raven","Lucy"] 
  putStrLn "------ Enjoy! ------"
  allArgs <- getArgs
  
  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt RequireOrder options allArgs

  -- Here we thread startOptions through all supplied option actions
  opts <- foldl (>>=) (return defaultOptions) actions

  let Options   {
    optHelp     = help,
    optWinner   = winner,
    optDepth    = searchDepth,
    optMove     = move,
    optVerbose  = verbose,
    optInterAct = interactive,
    fname        = fname
    } = opts

  

  when help (do printHelp; exitSuccess)
  when winner (printWinner fname)
  when (move /= "") (printMakeMove fname move) 
  when verbose (do printEvalMove searchDepth fname;)
  when interactive (startGame fname)
  
  

printHelp :: IO ()
printHelp = do
  putStrLn $ intercalate  "\n"  [
    "Here is HELP",
    "-h | --help                  Print flag options",
    "-w | --winner                Print best move with exhaustive search",
    "-d | --depth       INT       Set depth level of search",
    "-m | --move        Int,Int   Make move and print updated board",
    "-v | --verbose               Print best move in depth and a description of how good it is",
    "-i | --interactive           Start interactive game",
    "-f | --file        STRING    Assign file name (default: ???)"
    ]

printWinner :: String -> IO ()
printWinner fname = do
  game <- loadGame fname
  let bestLoc = bestMove game (-1)
  case bestLoc of
    Nothing -> putStrLn "No best move"
    Just x  -> putStrLn $ "Here is the best move:" ++ show x
  
printMakeMove :: String -> String ->  IO ()
printMakeMove fname move = do
  putStrLn "Here is the new board:"
  game <- loadGame fname
  loc <- readLocation move
  putStr $ showGameState game "(before)"
  -- putStr $ show $ head $ map snd (snd game)
  case makeMove game loc of
    Just x  -> do
      putStr $ showGameState x $ "Your input move is " ++ show loc -- move
      -- putStr $ show $ head $ map snd (snd x)
    Nothing -> putStr $ showGameState game $ "Illegal move (" ++ move ++"). No move is made."
  

printEvalMove :: Int -> String -> IO ()
-- VERBOSE
printEvalMove depth fname = do
  putStrLn "Here is Evaluation:"
  game <- loadGame fname
  -- lack of make the move
  let result = scoreGame game
  case result of 
    (Win x, _) -> putStrLn $ "Player " ++ show x ++ " has won!!!!!!!!\n"
    (Tie, 0 )   -> putStrLn "Tie!!!!!!"
    (Tie, s )   -> 
      if s > 0 then putStrLn $ "Player Cross is leading with score " ++ show s
      else putStrLn $ "Player Circle is leading with score " ++ show (-s)

startGame :: String -> IO ()
startGame fname = 
  putStrLn "Here is Start game. Future kids will build this."

readLocation :: String -> IO Location
readLocation str = do
  let lst = splitOn "," str
  case lst of 
    [] -> do
      putStrLn "Location input is missing. Try again!"
      putStr "Enter your new move as 'int,int' :"
      loc <- getLine
      readLocation loc
    [a,b] -> 
      if all (\x -> length x == 1 && (head x `elem` ['0'..'8'])) [a,b]
      then 
        do
          return (read a, read b)
      else 
        do
          putStrLn "indices in Location input is not within 0-8. Try again!"
          putStr "Enter your new move as 'int,int' :"
          loc <- getLine
          readLocation loc
    _ -> do
      putStrLn "Wrong format of location. Try again!"
      putStr "Enter your new move as 'int,int' :"
      loc <- getLine
      readLocation loc



printGame :: GameState -> IO()
printGame game = putStr $ showGameState game ""