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
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newContainers, newOutStack) = ( do
            if validateParameters outStack "exec"
                then (inpStack, deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                if not (isCODEBLOCK a)
                    then (inpStack, deallocateMemory a containers, ERROR ExpectedCodeblock : rest)
                else do
                    let block = containers Map.! getCODEBLOCK a
                    (block ++ inpStack, deallocateRootContainer a containers, rest))
    let result = (newInpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcEach :: StackState
funcEach = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newContainers, newOutStack) = ( do
            if validateParameters outStack "each"
                then (inpStack, deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                if not (isLIST a)
                    then (inpStack, deallocateStack [a,b] containers, ERROR ExpectedList : rest)
                else if not (isCODEBLOCK b) && not (isFUNC b) && not (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                    then (inpStack, deallocateStack [a,b] containers, ERROR ExpectedCodeblock : rest)
                else do
                    let list = containers Map.! getLIST a
                    let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                | otherwise = [b]
                    let (values, newContainers) = eachOf (reverse list) block ([], containers)
                    (values ++ inpStack, deallocateStack (a : block) newContainers, rest))
    let result = (newInpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


eachOf :: Stack -> Stack -> (Stack, Containers) -> (Stack, Containers)
eachOf [] _ (stack, containers) = (stack, containers)
eachOf (x:xs) block (stack, containers) = do
    let (dupBlock, newContainers) = duplicateStack block ([], containers)
    let (dupValue, containers) = duplicateValue x newContainers
    eachOf xs block (dupValue : dupBlock ++ stack, containers)
