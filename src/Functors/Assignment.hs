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
    let (newObjects, newVariables, newOutStack) = ( if length outStack < functors Map.! ":="
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (newObjects, variables, newOutStack)
                                                    else do
                                                        let (b:a:rest) = outStack
                                                        let newObjects = deallocateObject a objects
                                                        if not (isUNKNOWN a)
                                                            then (deallocateObject b newObjects, variables, ERROR ExpectedUnknown : rest)
                                                        else do
                                                            let newVariables = Map.insert (getUNKNOWN a) b variables
                                                            (newObjects, newVariables, rest)
                                                )
    put (inpStack, newObjects, newVariables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, newVariables, functions, newOutStack, statusIO)


funcSetFunction :: StackState
funcSetFunction = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newFunctions, newOutStack) = ( if length outStack < functors Map.! "fun"
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (newObjects, functions, newOutStack)
                                                    else do
                                                        let (b:a:rest) = outStack
                                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                                        if not (isUNKNOWN a)
                                                            then (newObjects, functions, ERROR ExpectedUnknown : rest)
                                                        else if not (isCODEBLOCK b)
                                                            then (newObjects, functions, ERROR ExpectedCodeblock : rest)
                                                        else do
                                                            let block = objects Map.! getCODEBLOCK b
                                                            let (dupBlock, newObjects) = duplicateStack block ([], objects)
                                                            let newFunctions = Map.insert (getUNKNOWN a) dupBlock functions
                                                            (deallocateObject a (deallocateObject b newObjects), newFunctions, rest)
                                                )
    put (inpStack, newObjects, variables, newFunctions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, newFunctions, newOutStack, statusIO)