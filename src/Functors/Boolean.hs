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
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "&&"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                let value   | not (isBOOL a) || not (isBOOL b) = ERROR ExpectedBool
                            | otherwise = BOOL (getBOOL a && getBOOL b)
                (deallocateStack [a, b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcOR :: StackState
funcOR = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "||"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                let value   | not (isBOOL a) || not (isBOOL b) = ERROR ExpectedBool
                            | otherwise = BOOL (getBOOL a || getBOOL b)
                (deallocateStack [a, b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcNOT :: StackState
funcNOT = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "not"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let value   | not (isBOOL a) = ERROR ExpectedBool
                            | otherwise = BOOL (not $ getBOOL a)
                (deallocateMemory a containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result
