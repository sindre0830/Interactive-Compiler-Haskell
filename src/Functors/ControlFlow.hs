module Functors.ControlFlow
    ( module Functors.ControlFlow
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import MemoryHandler (deallocateStack, duplicateStack)
import Converter (getBlock)

-- | Performs if operation and returns a value according to a boolean.
funcIf :: StackState
funcIf = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newContainers, newOutStack) = ( do
            if validateParameters outStack "if"
                then (inpStack, deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (c : b : a : rest) = outStack
                if not (isBOOL a)
                    then (inpStack, deallocateStack [a, b, c] containers, ERROR ExpectedBool : rest)
                else if getBOOL a
                    then do
                        let block = getBlock b
                        (block ++ inpStack, deallocateStack [a, c] containers, rest)
                else do
                    let block = getBlock c
                    (block ++ inpStack, deallocateStack [a, b] containers, rest))
    let result = (newInpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Performs a loop n times according to an integer and adds a value to the stack for each iteration.
funcTimes :: StackState
funcTimes = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newContainers, newOutStack) = ( do
            if validateParameters outStack "times"
                then (inpStack, deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                if not (isINT a) || getINT a < 0
                    then (inpStack, deallocateStack [a, b] containers, ERROR ExpectedPositiveInteger : rest)
                else do
                    let block = getBlock b
                    let (values, newContainers) = loopN (getINT a) block ([], containers)
                    (values ++ inpStack, deallocateStack (a : block) newContainers, rest))
    let result = (newInpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Adds stacks together n times.
loopN :: Integer -> Stack -> (Stack, Containers) -> (Stack, Containers)
loopN 0 _ (stack, containers) = (stack, containers)
loopN n block (stack, containers) = do
    let (dupBlock, newContainers) = duplicateStack block ([], containers)
    loopN (n - 1) block (dupBlock ++ stack, newContainers)
