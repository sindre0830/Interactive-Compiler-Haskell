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
    let result = (inpStack, objects, variables, functions, outStack, Input)
    put result >> return result


funcPrint :: StackState
funcPrint = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack, newStatusIO) = ( do
            if length outStack < functors Map.! "print"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount], statusIO)
            else do
                let (a:rest) = outStack
                if not (isSTRING a)
                    then (deallocateObject a objects, ERROR ExpectedString : rest, statusIO)
                else (deallocateObject a objects, PRINT (getSTRING a) : rest, Output))
    let result = (inpStack, newObjects, variables, functions, newOutStack, newStatusIO)
    put result >> return result
