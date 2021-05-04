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
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "pop"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let newObjects = deallocateObject a objects
                                            (rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)


funcDup :: StackState
funcDup = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "dup"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let (value, newObjects) = duplicateStack [a] ([], objects)
                                            (head value : outStack, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)


funcSwap :: StackState
funcSwap = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "swap"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            (a:b:rest, objects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)
