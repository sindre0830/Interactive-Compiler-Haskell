module Functors.ControlFlow
    ( module Functors.ControlFlow
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcIf :: StackState
funcIf = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newObjects, newOutStack) = ( do
            if validateParameters outStack "if"
                then (inpStack, deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (c:b:a:rest) = outStack
                if not (isBOOL a)
                    then (inpStack, deallocateStack [a,b,c] objects, ERROR ExpectedBool : rest)
                else if getBOOL a
                    then do
                        let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                    | otherwise = [b]
                        (block ++ inpStack, deallocateStack [a,c] objects, rest)
                else do
                    let block   | isCODEBLOCK c = [c, FUNC "exec"]
                                | otherwise = [c]
                    (block ++ inpStack, deallocateStack [a,b] objects, rest))
    let result = (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcTimes :: StackState
funcTimes = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newObjects, newOutStack) = ( do
            if validateParameters outStack "times"
                then (inpStack, deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                if not (isINT a) || getINT a < 0
                    then (inpStack, deallocateStack [a,b] objects, ERROR ExpectedPositiveInteger : rest)
                else do
                    let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                | otherwise = [b]
                    let (values, newObjects) = loopN (getINT a) block ([], objects)
                    (values ++ inpStack, deallocateStack (a : block) newObjects, rest))
    let result = (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


loopN :: Integer -> Stack -> (Stack, Objects) -> (Stack, Objects)
loopN 0 _ (stack, objects) = (stack, objects)
loopN n block (stack, objects) = do
    let (dupBlock, newObjects) = duplicateStack block ([], objects)
    loopN (n - 1) block (dupBlock ++ stack, newObjects)
