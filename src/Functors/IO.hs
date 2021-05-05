module Functors.IO
    ( module Functors.IO
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import MemoryHandler (deallocateMemory, deallocateStack)

-- | Performs read operation and adds a string to the stack.
funcRead :: StackState
funcRead = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    -- changes IO status to input to indicate a jump out of the executer
    let result = (inpStack, containers, variables, functions, outStack, Input)
    put result >> return result

-- | Performs print operation and prints a string on the stack.
funcPrint :: StackState
funcPrint = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack, newStatusIO) = ( do
            if validateParameters outStack "print"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount], statusIO)
            else do
                let (a : rest) = outStack
                if not (isSTRING a)
                    then (deallocateMemory a containers, ERROR ExpectedString : rest, statusIO)
                else (containers, outStack, Output))
    let result = (inpStack, newContainers, variables, functions, newOutStack, newStatusIO)
    put result >> return result
