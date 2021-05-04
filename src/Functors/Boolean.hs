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
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "&&"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b objects)
                let value   | not (isBOOL a) || not (isBOOL b) = ERROR ExpectedBool
                            | otherwise = BOOL (getBOOL a && getBOOL b)
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcOR :: StackState
funcOR = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "||"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b objects)
                let value   | not (isBOOL a) || not (isBOOL b) = ERROR ExpectedBool
                            | otherwise = BOOL (getBOOL a || getBOOL b)
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcNOT :: StackState
funcNOT = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "not"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (a:rest) = outStack
                let newObjects = deallocateObject a objects
                let value   | not (isBOOL a) = ERROR ExpectedBool
                            | otherwise = BOOL (not $ getBOOL a)
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
