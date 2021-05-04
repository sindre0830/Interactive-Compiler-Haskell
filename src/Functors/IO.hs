module Functors.IO
    ( module Functors.IO
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcRead :: StackState
funcRead = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    put (inpStack, objects, variables, functions, outStack, Input)
    return (inpStack, objects, variables, functions, outStack, Input)


funcPrint :: StackState
funcPrint = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects, newStatusIO) = (  if length outStack < functors Map.! "print"
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (newOutStack, newObjects, statusIO)
                                                    else do
                                                        let (a:rest) = outStack
                                                        let newObjects = deallocateObject a objects
                                                        if not (isSTRING a)
                                                            then (ERROR ExpectedString : rest, newObjects, statusIO)
                                                        else (PRINT (getSTRING a) : rest, newObjects, Output)
                                                )
    put (inpStack, newObjects, variables, functions, newOutStack, newStatusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, newStatusIO)
