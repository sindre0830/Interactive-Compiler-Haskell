module Functors.Boolean
    ( module Functors.Boolean
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcAND :: StackState
funcAND = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "&&"
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
            if length outStack < functors Map.! "||"
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
            if length outStack < functors Map.! "not"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                let value   | not (isBOOL a) = ERROR ExpectedBool
                            | otherwise = BOOL (not $ getBOOL a)
                (deallocateObject a objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
