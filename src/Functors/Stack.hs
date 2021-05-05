module Functors.Stack
    ( module Functors.Stack
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import MemoryHandler

-- | Removes a value from the stack.
funcPop :: StackState
funcPop = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "pop"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                (deallocateMemory a containers, rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Duplicate a value from the stack.
funcDup :: StackState
funcDup = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "dup"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let (value, newContainers) = duplicateValue a containers
                (newContainers, value : outStack))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Swap to values on the stack.
funcSwap :: StackState
funcSwap = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "swap"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                (containers, a : b : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result
