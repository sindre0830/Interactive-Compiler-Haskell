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
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    if null inpStack || statusIO /= None
        then do
            put (inpStack, objects, variables, functions, outStack, statusIO)
            return (inpStack, objects, variables, functions, outStack, statusIO)
    else do
        let (x:xs) = inpStack
        let (shouldSkip, newInpStack, newOutStack) = skipOperation inpStack variables functions
        if shouldSkip
            then do
                put (newInpStack, objects, variables, functions, newOutStack ++ outStack, statusIO) >> executeStack
        else if isFUNC x
            then ( do
                put (xs, objects, variables, functions, outStack, statusIO)
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
            let (moveToBuffer, newObjects, newOutStack) = setVariable [x] variables functions (False, objects, [])
            if moveToBuffer
                then put (reverse newOutStack ++ xs, newObjects, variables, functions, outStack, statusIO) >> executeStack
            else put (xs, newObjects, variables, functions, newOutStack ++ outStack, statusIO) >> executeStack


setVariable :: Stack -> Variables -> Functions -> (Bool, Objects, OutputStack) -> (Bool, Objects, OutputStack)
setVariable [] _ _ (moveToBuffer, objects, outStack) = (moveToBuffer, objects, outStack)
setVariable (x:xs) variables functions (moveToBuffer, objects, outStack)
    | isLIST x = do
        let key = getLIST x
        let list = objects Map.! key
        let (newMoveToBuffer, newObjects, newOutStack) = setVariable list variables functions (moveToBuffer, objects, [])
        let objects = updateObject key (reverse newOutStack) newObjects
        setVariable xs variables functions (newMoveToBuffer, objects, x : outStack)
    | isCODEBLOCK x = do
        let key = getCODEBLOCK x
        let block = objects Map.! key
        let (newMoveToBuffer, newObjects, newOutStack) = setVariable block variables functions (moveToBuffer, objects, [])
        let objects = updateObject key (reverse newOutStack) newObjects
        setVariable xs variables functions (newMoveToBuffer, objects, x : outStack)
    | isUNKNOWN x && Map.member (getUNKNOWN x) variables = do
        let value = variables Map.! getUNKNOWN x
        let (newStack, newObjects) = duplicateStack [value] ([], objects)
        setVariable xs variables functions (moveToBuffer, newObjects, newStack ++ outStack)
    | isUNKNOWN x && Map.member (getUNKNOWN x) functions = do
        let value = functions Map.! getUNKNOWN x
        let (newStack, newObjects) = duplicateStack value ([], objects)
        setVariable xs variables functions (True, newObjects, reverse newStack ++ outStack)
    | otherwise = setVariable xs variables functions (moveToBuffer, objects, x : outStack)


skipOperation :: Stack -> Variables -> Functions -> (Bool, Stack, Stack)
skipOperation stack variables functions
    | length stack >= 3 && isFUNC (stack !! 2) 
        && (getFUNC (stack !! 2) == "loop"
        || getFUNC (stack !! 2) == "if") = do
            let (x:y:rest) = stack
            (True, rest, y:[x])
    | length stack >= 2 && isFUNC (stack !! 1) 
        && (getFUNC (stack !! 1) == "map"
        || getFUNC (stack !! 1) == "each"
        || getFUNC (stack !! 1) == "loop"
        || getFUNC (stack !! 1) == "times"
        || getFUNC (stack !! 1) == "if"
        || getFUNC (stack !! 1) == "foldl") = do
            let (x:rest) = stack
            (True, rest, [x])
    | length stack >= 3 && isFUNC (stack !! 2) && isUNKNOWN (head stack) 
        && (Map.member (getUNKNOWN (head stack)) variables 
        || Map.member (getUNKNOWN (head stack)) functions)
        && (getFUNC (stack !! 2) == ":="
        || getFUNC (stack !! 2) == "fun") = do
            let (x:y:rest) = stack
            (True, rest, y:[x])
    | otherwise = (False, stack, [])


funcMap :: StackState
funcMap = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newVariables, newFunctions, newOutStack) = ( do   
            if length outStack < functors Map.! "map"
                then (deallocateStack outStack objects, variables, functions, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b objects)
                if not (isLIST a)
                    then (newObjects, variables, functions, ERROR ExpectedList : rest)
                else if not (isCODEBLOCK b) && not (isFUNC b) && not (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                    then (newObjects, variables, functions, ERROR ExpectedCodeblock : rest)
                else do
                    let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                | otherwise = [b]
                    let list = objects Map.! getLIST a
                    let (newObjects, newVariables, newFunctions, newList) = mapOf list block (objects, variables, functions, [])
                    let objects = deallocateStack block newObjects
                    if head newList == ERROR InvalidOperationIO
                        then do
                            (deallocateObject a objects, variables, functions, head newList : rest)
                    else do
                        let newObjects = updateObject (getLIST a) (reverse newList) objects
                        (newObjects, newVariables, newFunctions, a : rest))
    let result = (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)
    put result >> return result

mapOf :: Stack -> Stack -> (Objects, Variables, Functions, OutputStack) -> (Objects, Variables, Functions, OutputStack)
mapOf [] _ (objects, variables, functions, outStack) = (objects, variables, functions, outStack)
mapOf (x:xs) block (objects, variables, functions, outStack) = do
    let (dupBlock, newObjects) = duplicateStack block ([], objects)
    let (_, objects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newObjects, variables, functions, [x], None)
    if statusIO /= None
        then do
            let newObjects = deallocateStack outStack objects
            (newObjects, variables, functions, [ERROR InvalidOperationIO])
    else mapOf xs block (objects, newVariables, newFunctions, newOutStack ++ outStack)


funcFoldl :: StackState
funcFoldl = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newVariables, newFunctions, newOutStack) = ( do
            if length outStack < functors Map.! "foldl"
                then (deallocateStack outStack objects, variables, functions, [ERROR InvalidParameterAmount])
            else do
                let (c:b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b (deallocateObject c objects))
                if not (isLIST a)
                    then (newObjects, variables, functions, ERROR ExpectedList : rest)
                else if isCODEBLOCK b || isFUNC b  || (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                    then (newObjects, variables, functions, ERROR InvalidType : rest)
                else if not (isCODEBLOCK c) && not (isFUNC c) && not (isUNKNOWN c && Map.member (getUNKNOWN c) functions)
                    then (newObjects, variables, functions, ERROR ExpectedCodeblock : rest)
                else do
                    let block   | isCODEBLOCK c = [c, FUNC "exec"]
                                | otherwise = [c]
                    let list = objects Map.! getLIST a
                    let (newObjects, newVariables, newFunctions, newValue) = foldlOf list block (objects, variables, functions, b)
                    let objects = deallocateStack block newObjects
                    (deallocateObject a objects, newVariables, newFunctions, newValue : rest))
    let result = (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)
    put result >> return result


foldlOf :: Stack -> Stack -> (Objects, Variables, Functions, Type) -> (Objects, Variables, Functions, Type)
foldlOf [] _ (objects, variables, functions, value) = (objects, variables, functions, value)
foldlOf (x:xs) block (objects, variables, functions, value) = do
    let (dupBlock, newObjects) = duplicateStack block ([], objects)
    let (_, objects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newObjects, variables, functions, x : [value], None)
    if statusIO /= None
        then do
            let objects = deallocateStack dupBlock newObjects
            (deallocateObject value objects, variables, functions, ERROR InvalidOperationIO)
    else foldlOf xs block (objects, newVariables, newFunctions, head newOutStack)


funcLoop :: StackState
funcLoop = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "loop"
                then (inpStack, deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b objects)
                if not (isCODEBLOCK a) && not (isFUNC a)  && not (isUNKNOWN a && Map.member (getUNKNOWN a) functions) 
                    || not (isCODEBLOCK b) && not (isFUNC b) && not (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                    then (inpStack, newObjects, ERROR ExpectedCodeblock : rest)
                else do
                    let break   | isCODEBLOCK a = [a, FUNC "exec"]
                                | otherwise = [a]
                    let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                | otherwise = [b]
                    let (newObjects, values) = loop break block (objects, variables, functions, rest)
                    let objects = deallocateStack (break ++ block) newObjects
                    (values ++ inpStack, objects, []))
    let result = (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


loop :: Stack -> Stack -> (Objects, Variables, Functions, Stack) -> (Objects, Stack)
loop break block (objects, variables, functions, outStack) = do
    let (_, _, _, _, newOutStack, statusIO) = evalState executeStack (break, objects, variables, functions, outStack, None)
    let value = head newOutStack
    if statusIO /= None
        then (objects, [ERROR InvalidOperationIO])
    else if not (isBOOL value)
        then (objects, [ERROR ExpectedBool])
    else if getBOOL value
        then (objects, reverse outStack)
    else do
        let (dupBlock, newObjects) = duplicateStack block ([], objects)
        let (_, objects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newObjects, variables, functions, outStack, None)
        if statusIO /= None
            then (objects, [ERROR InvalidOperationIO])
        else loop break block (objects, newVariables, newFunctions, newOutStack)
