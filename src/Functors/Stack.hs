module Functors.Stack
    ( module Functors.Stack
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcPop :: StackState
funcPop = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "pop"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (a:rest) = outStack
                let newObjects = deallocateObject a objects
                (rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcDup :: StackState
funcDup = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "dup"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (a:rest) = outStack
                let (value, newObjects) = duplicateStack [a] ([], objects)
                (head value : outStack, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcSwap :: StackState
funcSwap = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "swap"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (b:a:rest) = outStack
                (a:b:rest, objects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
