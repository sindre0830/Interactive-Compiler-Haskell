module Functors.List
    ( module Functors.List
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcEmpty :: StackState
funcEmpty = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "empty"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let value   | not (isLIST a) = ERROR ExpectedList
                            | otherwise = BOOL (null (getContainer a containers))
                (deallocateMemory a containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcHead :: StackState
funcHead = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "head"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                if not (isLIST a)
                    then (deallocateMemory a containers, ERROR ExpectedList : rest)
                else do
                    let list = getContainer a containers
                    if null list
                        then (deallocateMemory a containers, rest)
                    else do
                        let (value, newContainers) = duplicateValue (head list) containers
                        (deallocateMemory a newContainers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcTail :: StackState
funcTail = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "tail"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                if not (isLIST a)
                    then (deallocateMemory a containers, ERROR ExpectedList : rest)
                else do
                    let list = getContainer a containers
                    let key = getLIST a
                    let newContainers   | null list = containers
                                        | otherwise = updateContainer key (tail list) containers
                    (deallocateMemory (head list) newContainers, outStack))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcCons :: StackState
funcCons = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "cons"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                if not (isLIST b)
                    then (deallocateStack [a, b] containers, ERROR ExpectedList : rest)
                else do
                    let key = getLIST b
                    let newContainers = updateContainer key (a : getContainer b containers) containers
                    (newContainers, b : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcAppend :: StackState
funcAppend = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "append"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                if not (isLIST a) || not (isLIST b)
                    then (deallocateStack [a, b] containers, ERROR ExpectedList : rest)
                else do
                    let keyB = getLIST b
                    let newContainers = updateContainer keyB (getContainer a containers ++ getContainer b containers) containers
                    (deallocateMemory a newContainers, b : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcLength :: StackState
funcLength = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "length"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let value   | isSTRING a = INT (toInteger $ length $ getSTRING a)
                            | isLIST a = INT (toInteger $ length $ getContainer a containers)
                            | isCODEBLOCK a = INT (toInteger $ length $ getContainer a containers)
                            | otherwise = ERROR ExpectedList
                (deallocateMemory a containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result
