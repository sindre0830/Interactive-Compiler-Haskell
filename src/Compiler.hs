module Compiler
    ( module Compiler
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (evalState, MonadState(put, get))
-- local modules
import Dictionary
import Stack
import Functors.Arithmetic
import Functors.Boolean
import Functors.Assignment
import Functors.IO
import Functors.Comparison
import Functors.Stack
import Functors.String
import Functors.List
import Functors.ControlFlow
import Functors.Other


executeStack :: StackState
executeStack = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    if null inpStack || searchForErrors outStack containers || statusIO /= None
        then return (inpStack, containers, variables, functions, outStack, statusIO)
    else do
        let (x : xs) = inpStack
        let (shouldSkip, newInpStack, newOutStack) = skipOperation inpStack variables functions
        if shouldSkip
            then put (newInpStack, containers, variables, functions, newOutStack ++ outStack, statusIO) >> executeStack
        else if isFUNC x
            then ( do
                put (xs, containers, variables, functions, outStack, statusIO)
                case getFUNC x of
                    -- Arithmetic
                    "+"     -> funcAddition
                    "-"     -> funcSubtraction
                    "*"     -> funcMultiplication
                    "/"     -> funcDivisionFloat
                    "div"   -> funcDivisionInteger
                    -- Bool
                    "&&"    -> funcAND
                    "||"    -> funcOR
                    "not"   -> funcNOT
                    -- Comparison
                    "=="    -> funcEqual
                    "<"     -> funcLess
                    ">"     -> funcGreater
                    -- Stack
                    "pop"   -> funcPop
                    "dup"   -> funcDup
                    "swap"  -> funcSwap
                    -- String
                    "parseInteger"  -> funcParseInteger
                    "parseFloat"    -> funcParseFloat
                    "words"         -> funcWords
                    -- List
                    "empty"     -> funcEmpty
                    "head"      -> funcHead
                    "tail"      -> funcTail
                    "cons"      -> funcCons
                    "append"    -> funcAppend
                    "length"    -> funcLength
                    -- Other
                    "exec"  -> funcExec
                    "map"   -> funcMap
                    "each"  -> funcEach
                    "foldl" -> funcFoldl
                    -- Control flow
                    "if"    -> funcIf
                    "times" -> funcTimes
                    "loop"  -> funcLoop
                    -- Assignment
                    ":="    -> funcSetVariable
                    "fun"   -> funcSetFunction
                    -- IO
                    "read"  -> funcRead
                    "print" -> funcPrint
                ) >> executeStack
        else do
            let (moveToBuffer, newContainers, newOutStack) = setVariable [x] variables functions (False, containers, [])
            if moveToBuffer
                then put (reverse newOutStack ++ xs, newContainers, variables, functions, outStack, statusIO) >> executeStack
            else put (xs, newContainers, variables, functions, newOutStack ++ outStack, statusIO) >> executeStack


searchForErrors :: Stack -> Containers -> Bool
searchForErrors [] _ = False
searchForErrors (x : xs) containers
    | isLIST x = do
        let list = containers `getContainer` x
        searchForErrors list containers || searchForErrors xs containers
    | isCODEBLOCK x = do
        let block = containers `getContainer` x
        searchForErrors block containers || searchForErrors block containers
    | otherwise = isERROR x


setVariable :: Stack -> Variables -> Functions -> (Bool, Containers, OutputStack) -> (Bool, Containers, OutputStack)
setVariable [] _ _ (moveToBuffer, containers, outStack) = (moveToBuffer, containers, outStack)
setVariable (x : xs) variables functions (moveToBuffer, containers, outStack)
    | isLIST x = do
        let list = containers `getContainer` x
        let (newMoveToBuffer, newContainers, newOutStack) = setVariable list variables functions (moveToBuffer, containers, [])
        let containers = updateContainer x (reverse newOutStack) newContainers
        setVariable xs variables functions (newMoveToBuffer, containers, x : outStack)
    | isCODEBLOCK x = do
        let block = containers `getContainer` x
        let (newMoveToBuffer, newContainers, newOutStack) = setVariable block variables functions (moveToBuffer, containers, [])
        let containers = updateContainer x (reverse newOutStack) newContainers
        setVariable xs variables functions (newMoveToBuffer, containers, x : outStack)
    | isVariable x variables = do
        let value = variables Map.! getUNKNOWN x
        let (newStack, newContainers) = duplicateStack [value] ([], containers)
        setVariable xs variables functions (moveToBuffer, newContainers, newStack ++ outStack)
    | isFunction x functions = do
        let value = functions Map.! getUNKNOWN x
        let (newStack, newContainers) = duplicateStack value ([], containers)
        setVariable xs variables functions (True, newContainers, reverse newStack ++ outStack)
    | otherwise = setVariable xs variables functions (moveToBuffer, containers, x : outStack)


skipOperation :: Stack -> Variables -> Functions -> (Bool, Stack, Stack)
skipOperation stack variables functions
    | length stack >= 3 && isFUNC (stack !! 2)
        && (getFUNC (stack !! 2) == "loop"
        || getFUNC (stack !! 2) == "if") = do
            let (x : y : rest) = stack
            (True, rest, [y, x])
    | length stack >= 2 && isFUNC (stack !! 1)
        && (getFUNC (stack !! 1) == "map"
        || getFUNC (stack !! 1) == "each"
        || getFUNC (stack !! 1) == "loop"
        || getFUNC (stack !! 1) == "times"
        || getFUNC (stack !! 1) == "if"
        || getFUNC (stack !! 1) == "foldl") = do
            let (x : rest) = stack
            (True, rest, [x])
    | length stack >= 3 && isFUNC (stack !! 2) 
        && (isVariable (head stack) variables || isFunction (head stack) functions)
        && (getFUNC (stack !! 2) == ":="
        || getFUNC (stack !! 2) == "fun") = do
            let (x : y : rest) = stack
            (True, rest, [y, x])
    | otherwise = (False, stack, [])


funcMap :: StackState
funcMap = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newVariables, newFunctions, newOutStack) = ( do
            if validateParameters outStack "map"
                then (deallocateStack outStack containers, variables, functions, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                if not (isLIST a)
                    then (deallocateStack [a, b] containers, variables, functions, ERROR ExpectedList : rest)
                else if not (isCODEBLOCK b) && not (isFUNC b) && not (isFunction b functions)
                    then (deallocateStack [a, b] containers, variables, functions, ERROR ExpectedCodeblock : rest)
                else do
                    let block = getBlock b
                    let list = containers `getContainer` a
                    let (newContainers, newVariables, newFunctions, newList) = mapOf list block (containers, variables, functions, [])
                    let containers = deallocateStack block newContainers
                    if head newList == ERROR InvalidOperationIO
                        then (deallocateMemory a containers, variables, functions, head newList : rest)
                    else do
                        let newContainers = updateContainer a (reverse newList) containers
                        (newContainers, newVariables, newFunctions, a : rest))
    let result = (inpStack, newContainers, newVariables, newFunctions, newOutStack, statusIO)
    put result >> return result

mapOf :: Stack -> Stack -> (Containers, Variables, Functions, OutputStack) -> (Containers, Variables, Functions, OutputStack)
mapOf [] _ (containers, variables, functions, outStack) = (containers, variables, functions, outStack)
mapOf (x : xs) block (containers, variables, functions, outStack) = do
    let (dupBlock, newContainers) = duplicateStack block ([], containers)
    let (_, containers, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newContainers, variables, functions, [x], None)
    if statusIO /= None
        then (deallocateStack outStack containers, variables, functions, [ERROR InvalidOperationIO])
    else mapOf xs block (containers, newVariables, newFunctions, newOutStack ++ outStack)


funcFoldl :: StackState
funcFoldl = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newVariables, newFunctions, newOutStack) = ( do
            if validateParameters outStack "foldl"
                then (deallocateStack outStack containers, variables, functions, [ERROR InvalidParameterAmount])
            else do
                let (c : b : a : rest) = outStack
                if not (isLIST a)
                    then (deallocateStack [a, b, c] containers, variables, functions, ERROR ExpectedList : rest)
                else if isCODEBLOCK b || isFUNC b  || isFunction b functions
                    then (deallocateStack [a, b, c] containers, variables, functions, ERROR InvalidType : rest)
                else if not (isCODEBLOCK c) && not (isFUNC c) && not (isFunction c functions)
                    then (deallocateStack [a, b, c] containers, variables, functions, ERROR ExpectedCodeblock : rest)
                else do
                    let block = getBlock c
                    let list = containers `getContainer` a
                    let (newContainers, newVariables, newFunctions, newValue) = foldlOf list block (containers, variables, functions, b)
                    (deallocateStack (a : block) newContainers, newVariables, newFunctions, newValue : rest))
    let result = (inpStack, newContainers, newVariables, newFunctions, newOutStack, statusIO)
    put result >> return result


foldlOf :: Stack -> Stack -> (Containers, Variables, Functions, Type) -> (Containers, Variables, Functions, Type)
foldlOf [] _ (containers, variables, functions, value) = (containers, variables, functions, value)
foldlOf (x : xs) block (containers, variables, functions, value) = do
    let (dupBlock, newContainers) = duplicateStack block ([], containers)
    let (_, containers, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newContainers, variables, functions, [x, value], None)
    if statusIO /= None
        then (deallocateStack (value : dupBlock) newContainers, variables, functions, ERROR InvalidOperationIO)
    else foldlOf xs block (containers, newVariables, newFunctions, head newOutStack)


funcLoop :: StackState
funcLoop = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newContainers, newOutStack) = ( do
            if validateParameters outStack "loop"
                then (inpStack, deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                if not (isCODEBLOCK a) && not (isFUNC a)  && not (isFunction a functions)
                    || not (isCODEBLOCK b) && not (isFUNC b) && not (isFunction b functions)
                    then (inpStack, deallocateStack [a, b] containers, ERROR ExpectedCodeblock : rest)
                else do
                    let break = getBlock a
                    let block = getBlock b
                    let (newContainers, values) = loop break block (containers, variables, functions, rest)
                    (values ++ inpStack, deallocateStack (break ++ block) newContainers, []))
    let result = (newInpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


loop :: Stack -> Stack -> (Containers, Variables, Functions, Stack) -> (Containers, Stack)
loop break block (containers, variables, functions, outStack) = do
    let (_, _, _, _, newOutStack, statusIO) = evalState executeStack (break, containers, variables, functions, outStack, None)
    let value = head newOutStack
    if statusIO /= None
        then (containers, [ERROR InvalidOperationIO])
    else if not (isBOOL value)
        then (containers, [ERROR ExpectedBool])
    else if getBOOL value
        then (containers, reverse outStack)
    else do
        let (dupBlock, newContainers) = duplicateStack block ([], containers)
        let (_, containers, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newContainers, variables, functions, outStack, None)
        if statusIO /= None
            then (containers, [ERROR InvalidOperationIO])
        else loop break block (containers, newVariables, newFunctions, newOutStack)
