module UI
    ( module UI
    ) where
-- foreign modules
import System.IO
import System.Directory
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
-- local modules
import Dictionary
import Stack
import Parser
import Compiler
import Convert

-- | Adds a flush input message to the user.
readInput :: String -> IO ()
readInput message = do
    putStr message
    hFlush stdout

-- | Menu of the program.
menu :: IO ()
menu = do
    readInput "bprog2 > "
    input <- getLine
    let cmd = stringToLower input
    if cmd == "interactive"
        then do
            putStrLn "Starting interactive mode...\n"
            modeInteractive ([], Map.empty, Map.empty, Map.empty, [], None) False
    else if cmd == "compiler"
        then do
            files <- listDirectory "documents"
            if null files
                then putStrLn "No files available. Terminating program...\n"
            else do
                putStrLn $ "\nAvailable file(s): " ++ intercalate " | " files
                readInput "Type in filename > "
                input <- getLine
                getFileName input files
    else menu

-- | Gets valid file name from the user.
getFileName :: String -> [FilePath] -> IO ()
getFileName input files
    | input `elem` files = do
        putStrLn "Starting compiler mode...\n"
        modeCompiler input ([], Map.empty, Map.empty, Map.empty, [], None) False
    | otherwise = do
        putStrLn "\nCouldn't find file, try again...\n"
        putStrLn $ "Available file(s): " ++ intercalate " | " files
        readInput "Type in filename > "
        input <- getLine
        getFileName input files

-- | Handler for the interactive mode. Allows for continius usage after stack has been executed.
modeInteractive :: (InputStack, Containers, Variables, Functions, OutputStack, StatusIO) -> Bool -> IO ()
modeInteractive (inpStack, containers, variables, functions, outStack, statusIO) showStack = do
    if statusIO == Output
        then do
            let (x:rest) = outStack
            putStrLn $ "output > " ++ getSTRING x
            let (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (inpStack, containers, variables, functions, rest, None)
            modeInteractive (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) True
    else if statusIO == Input
        then do
            readInput "input > "
            input <- getLine
            let value = STRING input
            let (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (inpStack, containers, variables, functions, value : outStack, None)
            modeInteractive (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) True
    else if showStack
        then if searchForErrors outStack containers
            then do
                putStrLn "FOUND ERROR!"
                renderLogsAfterExecute (inpStack, containers, outStack)
                putStrLn "Reverting back to a stable version of the program...\n"
                modeInteractive ([], deallocateStack outStack containers, variables, functions, [], statusIO) False
        else do
            putStrLn $ "Stack: " ++ printableStack (inpStack, containers, variables, functions, outStack, statusIO) ++ "\n"
            modeInteractive (inpStack, containers, variables, functions, outStack, statusIO) False
    else do
        readInput "bprog2 > "
        input <- getLine
        putStrLn ""
        if stringToLower input == "--debug"
            then do
                putStrLn $ "Stack:      " ++ show outStack
                putStrLn $ "Containers: " ++ show (Map.toList containers)
                putStrLn $ "Variables:  " ++ show (Map.toList variables)
                putStrLn $ "Functions:  " ++ show (Map.toList functions) ++ "\n"
                modeInteractive (inpStack, containers, variables, functions, outStack, statusIO) False
        else do
            let tokens = tokenize input
            let (parsedInpStack, parsedContainers) = parser tokens inpStack containers
            let (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (parsedInpStack, parsedContainers, variables, functions, outStack, None)
            if searchForErrors newOutStack newContainers
                then do
                    putStrLn "FOUND ERROR!"
                    renderLogsBeforeExecute (parsedInpStack, parsedContainers, outStack)
                    renderLogsAfterExecute (newInpStack, newContainers, newOutStack)
                    putStrLn "Reverting back to a stable version of the program...\n"
                    modeInteractive (inpStack, containers, variables, functions, outStack, statusIO) True
            else modeInteractive (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) True

-- | Handler for the compiler mode. Executes a text file and ends the program upon errors or end of the file.
modeCompiler :: FilePath -> (InputStack, Containers, Variables, Functions, OutputStack, StatusIO) -> Bool -> IO ()
modeCompiler file (inpStack, containers, variables, functions, outStack, statusIO) showStack = do
    if statusIO == Output
        then do
            let (x:rest) = outStack
            putStrLn $ "output: " ++ getSTRING x
            let (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (inpStack, containers, variables, functions, rest, None)
            modeCompiler file (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) True
    else if statusIO == Input
        then do
            readInput "input > "
            input <- getLine
            let value = STRING input
            let (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (inpStack, containers, variables, functions, value : outStack, None)
            modeCompiler file (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) True
    else if showStack
        then if searchForErrors outStack containers
            then do
                putStrLn "FOUND ERROR!"
                renderLogsAfterExecute (inpStack, containers, outStack)
                putStrLn "Terminating program...\n"
        else putStrLn $ "Stack: " ++ printableStack (inpStack, containers, variables, functions, outStack, statusIO) ++ "\n"
    else do
        handle <- openFile ("documents/" ++ file) ReadMode
        contents <- hGetContents handle
        let tokens = tokenize contents
        let (parsedInpStack, parsedContainers) = parser tokens inpStack containers
        let (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) = evalState executeStack (parsedInpStack, parsedContainers, variables, functions, outStack, None)
        if searchForErrors newOutStack newContainers
            then do
                putStrLn "FOUND ERROR!"
                renderLogsBeforeExecute (parsedInpStack, parsedContainers, outStack)
                renderLogsAfterExecute (newInpStack, newContainers, newOutStack)
                putStrLn "Terminating program...\n"
        else modeCompiler file (newInpStack, newContainers, newVariables, newFunctions, newOutStack, newStatusIO) True

-- | Renders logs of the stack before it was executed.
renderLogsBeforeExecute :: (InputStack, Containers, OutputStack) -> IO ()
renderLogsBeforeExecute (beforeInpStack, beforeContainers, beforeOutStack) = do
    putStrLn "    Logs before execute:"
    putStrLn $ "        Buffer  " ++ printableStack ([], beforeContainers, Map.empty, Map.empty, beforeInpStack, None)
    putStrLn $ "        Stack   " ++ printableStack ([], beforeContainers, Map.empty, Map.empty, beforeOutStack, None)

-- | Renders logs of the stack after it was executed.
renderLogsAfterExecute :: (InputStack, Containers, OutputStack) -> IO ()
renderLogsAfterExecute (afterInpStack, afterContainers, afterOutStack) = do
    putStrLn "    Logs after execute:"
    putStrLn $ "        Buffer  " ++ printableStack ([], afterContainers, Map.empty, Map.empty, afterInpStack, None)
    putStrLn $ "        Stack   " ++ printableStack ([], afterContainers, Map.empty, Map.empty, afterOutStack, None)

-- | Function to test the compiler. Used in Spec.hs
testCompiler :: String -> String
testCompiler input = do
    let tokens = tokenize input
    let (newInpStack, newContainers) = parser tokens [] Map.empty
    printableStack $ evalState executeStack (newInpStack, newContainers, Map.empty, Map.empty, [], None)
