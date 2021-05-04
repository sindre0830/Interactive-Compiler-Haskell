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
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newVariables, newOutStack) = ( do
            if validateParameters outStack ":="
                then (deallocateStack outStack objects, variables, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                if not (isUNKNOWN a)
                    then (deallocateStack [a,b] objects, variables, ERROR ExpectedUnknown : rest)
                else do
                    let newVariables = Map.insert (getUNKNOWN a) b variables
                    (deallocateObject a objects, newVariables, rest))
    let result = (inpStack, newObjects, newVariables, functions, newOutStack, statusIO)
    put result >> return result


funcSetFunction :: StackState
funcSetFunction = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newFunctions, newOutStack) = ( do
            if validateParameters outStack "fun"
                then (deallocateStack outStack objects, functions, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                if not (isUNKNOWN a)
                    then (deallocateStack [a,b] objects, functions, ERROR ExpectedUnknown : rest)
                else if not (isCODEBLOCK b)
                    then (deallocateStack [a,b] objects, functions, ERROR ExpectedCodeblock : rest)
                else do
                    let block = objects Map.! getCODEBLOCK b
                    let (dupBlock, newObjects) = duplicateStack block ([], objects)
                    let newFunctions = Map.insert (getUNKNOWN a) dupBlock functions
                    (deallocateStack [a,b] newObjects, newFunctions, rest))
    let result = (inpStack, newObjects, variables, newFunctions, newOutStack, statusIO)
    put result >> return result
