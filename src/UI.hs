module UI
    ( menu
    ) where
-- foreign modules
import System.IO ( hFlush, stdout )
import Data.Char ( toLower )
import Data.Map (Map)
import qualified Data.Map as Map
-- local modules
import Dictionary
import Stack
import Parsing

readInput :: IO ()
readInput = do
    putStr "bprog: "
    hFlush stdout

menu :: IO ()
menu = do
    readInput
    input <- getLine
    let cmd = stringToLower input
    if cmd == "interactive"
        then do
            putStrLn "Starting interactive mode..."
            let variables = Map.empty
            modeInteractive [] variables
    else if cmd == "compiler"
        then do
            putStrLn "Starting compiler mode..."
            let variables = Map.empty
            modeCompiler variables
    else menu

modeInteractive :: Stack -> Variable -> IO ()
modeInteractive stack variables = do
    readInput
    input <- getLine
    let tokens = tokenize input
    let (newStack, objects) = parser tokens [] Map.empty
    putStrLn "\n\tBefore compiling:"
    putStrLn $ "\t\tRaw:   " ++ show newStack
    putStrLn $ "\t\tStack: " ++ printableStack objects newStack
    -- let execStack = executePrefix newStack variables
    -- putStrLn "\n\tAfter compiling:"
    -- putStrLn $ "\t\tRaw:   " ++ show execStack
    -- putStrLn $ "\t\tStack: " ++ printableStack execStack variables ++ "\n"
    modeInteractive [] variables

modeCompiler :: Variable -> IO ()
modeCompiler variables = do
    readInput
    input <- getLine
    modeCompiler variables

-- | Converts string to lowercase.
stringToLower :: String -> String
stringToLower = map toLower
