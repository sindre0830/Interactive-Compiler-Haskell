module UI
    ( menu
    ) where
-- foreign modules
import System.IO
import Data.Char ( toLower )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
-- local modules
import Dictionary
import Stack
import Parsing
import Compiler

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
            modeInteractive [] Map.empty
    else if cmd == "compiler"
        then do
            putStrLn "Starting compiler mode..."
            modeCompiler
    else menu

modeInteractive :: Stack -> Variables -> IO ()
modeInteractive stack variables = do
    readInput
    input <- getLine
    let tokens = tokenize input
    let (beforeStack, objects) = parser tokens [] Map.empty
    putStrLn "\n\tBefore compiling:"
    putStrLn $ "\t\tRaw:   " ++ show beforeStack
    putStrLn $ "\t\tStack: " ++ printableStack ([], objects, Map.empty, Map.empty, beforeStack, False)
    let (_, newObjects, _, _, afterstack, _) = evalState executeStack (beforeStack, objects, Map.empty, Map.empty, [], False)
    putStrLn "\n\tAfter compiling:"
    putStrLn $ "\t\tRaw:   " ++ show afterstack
    putStrLn $ "\t\tStack: " ++ printableStack ([], newObjects, Map.empty, Map.empty, afterstack, False) ++ "\n"
    modeInteractive [] Map.empty

modeCompiler :: IO ()
modeCompiler = do
    handle <- openFile "documents/test.txt" ReadMode
    contents <- hGetContents handle
    let tokens = tokenize contents
    let (beforeStack, objects) = parser tokens [] Map.empty
    putStrLn "\n\tBefore compiling:"
    putStrLn $ "\t\tRaw:   " ++ show beforeStack
    putStrLn $ "\t\tStack: " ++ printableStack ([], objects, Map.empty, Map.empty, beforeStack, False)
    let (_, newObjects, _, _, afterstack, _) = evalState executeStack (beforeStack, objects, Map.empty, Map.empty, [], False)
    putStrLn "\n\tAfter compiling:"
    putStrLn $ "\t\tRaw:   " ++ show afterstack
    putStrLn $ "\t\tStack: " ++ printableStack ([], newObjects, Map.empty, Map.empty, afterstack, False) ++ "\n"

-- | Converts string to lowercase.
stringToLower :: String -> String
stringToLower = map toLower
