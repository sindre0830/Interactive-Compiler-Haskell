module Functors.Assignment
    ( module Functors.Assignment
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcSetVariable :: StackState
funcSetVariable = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newVariables, newOutStack) = ( do
            if validateParameters outStack ":="
                then (deallocateStack outStack containers, variables, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                if not (isUNKNOWN a)
                    then (deallocateStack [a, b] containers, variables, ERROR ExpectedUnknown : rest)
                else do
                    let newVariables = Map.insert (getUNKNOWN a) b variables
                    (deallocateMemory a containers, newVariables, rest))
    let result = (inpStack, newContainers, newVariables, functions, newOutStack, statusIO)
    put result >> return result


funcSetFunction :: StackState
funcSetFunction = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newFunctions, newOutStack) = ( do
            if validateParameters outStack "fun"
                then (deallocateStack outStack containers, functions, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                if not (isUNKNOWN a)
                    then (deallocateStack [a, b] containers, functions, ERROR ExpectedUnknown : rest)
                else if not (isCODEBLOCK b)
                    then (deallocateStack [a, b] containers, functions, ERROR ExpectedCodeblock : rest)
                else do
                    let block = getBlock b
                    let newFunctions = Map.insert (getUNKNOWN a) block functions
                    (deallocateMemory a containers, newFunctions, rest))
    let result = (inpStack, newContainers, variables, newFunctions, newOutStack, statusIO)
    put result >> return result
