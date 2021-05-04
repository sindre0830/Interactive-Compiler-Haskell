module Functors.Boolean
    ( module Functors.Boolean
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcAND :: StackState
funcAND = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "&&"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | not (isBOOL a) || not (isBOOL b) = ERROR ExpectedBool
                            | otherwise = BOOL (getBOOL a && getBOOL b)
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcOR :: StackState
funcOR = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "||"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | not (isBOOL a) || not (isBOOL b) = ERROR ExpectedBool
                            | otherwise = BOOL (getBOOL a || getBOOL b)
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcNOT :: StackState
funcNOT = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "not"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                let value   | not (isBOOL a) = ERROR ExpectedBool
                            | otherwise = BOOL (not $ getBOOL a)
                (deallocateObject a objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
