module UI
    ( module UI
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

readInput :: String -> IO ()
readInput message = do
    putStr (message ++ ": ")
    hFlush stdout

menu :: IO ()
menu = do
    readInput "bprog2"
    input <- getLine
    let cmd = stringToLower input
    if cmd == "interactive"
        then do
            putStrLn "Starting interactive mode...\n"
            modeInteractive ([], Map.empty, Map.empty, Map.empty, [], None) False
    else if cmd == "compiler"
        then do
            putStrLn "Starting compiler mode...\n"
            modeCompiler ([], Map.empty, Map.empty, Map.empty, [], None) False
    else menu

modeInteractive :: (InputStack, Objects, Variables, Functions, OutputStack, StatusIO) -> Bool -> IO ()
modeInteractive (inpStack, objects, variables, functions, outStack, statusIO) showStack = do
    if statusIO == Output
        then do
            let (x:rest) = outStack
            putStrLn $ "output: " ++ getSTRING x
            let (newInpStack, newObjects, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (inpStack, objects, variables, functions, rest, None)
            modeInteractive (newInpStack, newObjects, newVariables, newFunctions, newOutStack, newStatusIO) True
    else if statusIO == Input
        then do
            readInput "input"
            input <- getLine
            let value = STRING input
            let (newInpStack, newObjects, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (inpStack, objects, variables, functions, value : outStack, None)
            modeInteractive (newInpStack, newObjects, newVariables, newFunctions, newOutStack, newStatusIO) True
    else if showStack
        then do
            putStrLn $ "Stack: " ++ printableStack (inpStack, objects, variables, functions, outStack, statusIO) ++ "\n"
            modeInteractive (inpStack, objects, variables, functions, outStack, statusIO) False
    else do
        readInput "bprog2"
        input <- getLine
        putStrLn ""
        if stringToLower input == "--debug"
            then do
                putStrLn $ "Stack:     " ++ show outStack
                putStrLn $ "Objects:   " ++ show objects
                putStrLn $ "Variables: " ++ show variables
                putStrLn $ "Functions: " ++ show functions ++ "\n"
                modeInteractive (inpStack, objects, variables, functions, outStack, statusIO) False
        else do
            let tokens = tokenize input
            let (newInpStack, newObjects) = parser tokens inpStack objects
            let (inpStack, objects, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (newInpStack, newObjects, variables, functions, outStack, None)
            modeInteractive (inpStack, objects, newVariables, newFunctions, newOutStack, newStatusIO) True

modeCompiler :: (InputStack, Objects, Variables, Functions, OutputStack, StatusIO) -> Bool -> IO ()
modeCompiler (inpStack, objects, variables, functions, outStack, statusIO) showStack = do
    if statusIO == Output
        then do
            let (x:rest) = outStack
            putStrLn $ "output: " ++ getSTRING x
            let (newInpStack, newObjects, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (inpStack, objects, variables, functions, rest, None)
            modeCompiler (newInpStack, newObjects, newVariables, newFunctions, newOutStack, newStatusIO) True
    else if statusIO == Input
        then do
            readInput "input"
            input <- getLine
            let value = STRING input
            let (newInpStack, newObjects, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (inpStack, objects, variables, functions, value : outStack, None)
            modeCompiler (newInpStack, newObjects, newVariables, newFunctions, newOutStack, newStatusIO) True
    else if showStack
        then putStrLn $ "Stack: " ++ printableStack (inpStack, objects, variables, functions, outStack, statusIO) ++ "\n"
    else do
        handle <- openFile "documents/test.txt" ReadMode
        contents <- hGetContents handle
        let tokens = tokenize contents
        let (newInpStack, newObjects) = parser tokens inpStack objects
        let (inpStack, objects, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (newInpStack, newObjects, variables, functions, outStack, None)
        modeCompiler (inpStack, objects, newVariables, newFunctions, newOutStack, newStatusIO) True

-- | Converts string to lowercase.
stringToLower :: String -> String
stringToLower = map toLower

testCompiler :: String -> String
testCompiler input = do
    let tokens = tokenize input
    let (newInpStack, newObjects) = parser tokens [] Map.empty
    printableStack $ evalState executeStack (newInpStack, newObjects, Map.empty, Map.empty, [], None)
