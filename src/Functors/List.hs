module Functors.List
    ( module Functors.List
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import MemoryHandler (deallocateMemory, deallocateStack, duplicateValue, getContainer, updateContainer, deallocateRootContainer)

-- | Performs empty operation by checking if there are any elements in the list.
funcEmpty :: StackState
funcEmpty = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "empty"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let value   | not (isLIST a)    = ERROR ExpectedList
                            | otherwise         = BOOL (null (containers `getContainer` a))
                (deallocateMemory a containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Gets the first value in the list and puts it on the stack.
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
                    let list = containers `getContainer` a
                    -- allows head on an empty list (does nothing)
                    if null list
                        then (deallocateMemory a containers, rest)
                    else do
                        let (value, newContainers) = duplicateValue (head list) containers
                        (deallocateMemory a newContainers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Gets the tail of a list and puts it on the stack.
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
                    let list = containers `getContainer` a
                    -- allows tail on an empty list (does nothing)
                    let newContainers   | null list = containers
                                        | otherwise = updateContainer a (tail list) containers
                    (deallocateMemory (head list) newContainers, outStack))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Adds a value to the start of a list.
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
                    let newContainers = updateContainer b (a : containers `getContainer` b) containers
                    (newContainers, b : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Appends a list to another list.
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
                    let newContainers = updateContainer b (containers `getContainer` a ++ containers `getContainer` b) containers
                    (deallocateRootContainer a newContainers, b : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Gets the lenght of a list.
funcLength :: StackState
funcLength = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "length"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let value   | isSTRING a                = INT (toInteger $ length $ getSTRING a)
                            | isLIST a || isCODEBLOCK a = INT (toInteger $ length $ containers `getContainer` a)
                            | otherwise                 = ERROR ExpectedList
                (deallocateMemory a containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result
