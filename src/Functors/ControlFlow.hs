module Functors.ControlFlow
    ( module Functors.ControlFlow
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcIf :: StackState
funcIf = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "if"
                then do
                    let (newOutStack, newObjects) = deallocateStack outStack objects
                    (inpStack, newOutStack, newObjects)
            else do
                let (c:b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b (deallocateObject c objects))
                if not (isBOOL a)
                    then (inpStack, ERROR ExpectedBool : rest, newObjects)
                else if getBOOL a
                    then do
                        let values  | isCODEBLOCK b = [b, FUNC "exec"]
                                    | otherwise = [b]
                        (values ++ inpStack, rest, deallocateObject a (deallocateObject c objects))
                else do
                    let values  | isCODEBLOCK c = [c, FUNC "exec"]
                                | otherwise = [c]
                    (values ++ inpStack, rest, deallocateObject a (deallocateObject b objects)))
    put (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (newInpStack, newObjects, variables, functions, newOutStack, statusIO)


funcTimes :: StackState
funcTimes = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "times"
                then do
                    let (newStack, newObjects) = deallocateStack outStack objects
                    (inpStack, newObjects, newStack)
            else do
                let (b:a:rest) = outStack
                let newObject = deallocateObject a (deallocateObject b objects)
                if not (isINT a) || getINT a < 0
                    then (inpStack, newObject, ERROR ExpectedPositiveInteger : rest)
                else do
                    let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                | otherwise = [b]
                    let (values, newObjects) = loopN (getINT a) block ([], objects)
                    let (_, objects) = deallocateStack block newObjects
                    (values ++ inpStack, deallocateObject a objects, rest))
    put (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (newInpStack, newObjects, variables, functions, newOutStack, statusIO)


loopN :: Integer -> Stack -> (Stack, Objects) -> (Stack, Objects)
loopN 0 _ (stack, objects) = (stack, objects)
loopN n block (stack, objects) = do
    let (dupBlock, newObjects) = duplicateStack block ([], objects)
    loopN (n - 1) block (dupBlock ++ stack, newObjects)
