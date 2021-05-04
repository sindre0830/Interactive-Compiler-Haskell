module Functors.Stack
    ( module Functors.Stack
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcPop :: StackState
funcPop = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "pop"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                (deallocateObject a objects, rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcDup :: StackState
funcDup = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "dup"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                let (value, newObjects) = duplicateObject a objects
                (newObjects, value : outStack))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcSwap :: StackState
funcSwap = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "swap"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                (objects, a:b:rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
