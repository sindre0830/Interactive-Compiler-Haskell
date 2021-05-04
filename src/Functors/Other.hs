module Functors.Other
    ( module Functors.Other
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcExec :: StackState
funcExec = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newOutStack, newObjects) = (  if length outStack < functors Map.! "exec"
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (inpStack, newOutStack, newObjects)
                                                    else do
                                                        let (a:rest) = outStack
                                                        if not (isCODEBLOCK a)
                                                            then (inpStack, ERROR ExpectedCodeblock : rest, deallocateObject a objects)
                                                        else do
                                                            let block = objects Map.! getCODEBLOCK a
                                                            (block ++ inpStack, rest, deallocateOneObject a objects)
                                                )
    put (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
