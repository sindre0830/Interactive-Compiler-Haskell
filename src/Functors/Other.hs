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
    let (newInpStack, newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "exec"
                then do
                    let (newOutStack, newObjects) = deallocateStack outStack objects
                    (inpStack, newOutStack, newObjects)
            else do
                let (a:rest) = outStack
                if not (isCODEBLOCK a)
                    then (inpStack, ERROR ExpectedCodeblock : rest, deallocateObject a objects)
                else do
                    let block = objects Map.! getCODEBLOCK a
                    (block ++ inpStack, rest, deallocateOneObject a objects))
    let result = (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcEach :: StackState
funcEach = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "each"
                then do
                    let (newStack, newObjects) = deallocateStack outStack objects
                    (inpStack, newObjects, newStack)
            else do
                let (b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b objects)
                if not (isLIST a)
                    then (inpStack, newObjects, ERROR ExpectedList : rest)
                else if not (isCODEBLOCK b) && not (isFUNC b) && not (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                    then (inpStack, newObjects, ERROR ExpectedCodeblock : rest)
                else do
                    let list = objects Map.! getLIST a
                    let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                | otherwise = [b]
                    let (values, newObjects) = eachOf (reverse list) block ([], objects)
                    let (_, objects) = deallocateStack block newObjects
                    (values ++ inpStack, deallocateObject a objects, rest))
    let result = (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


eachOf :: Stack -> Stack -> (Stack, Objects) -> (Stack, Objects)
eachOf [] _ (stack, objects) = (stack, objects)
eachOf (x:xs) block (stack, objects) = do
    let (dupBlock, newObjects) = duplicateStack block ([], objects)
    let (dupValue, objects) = duplicateObject x newObjects
    eachOf xs block (dupValue : dupBlock ++ stack, objects)
