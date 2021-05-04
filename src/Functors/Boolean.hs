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
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "&&"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject a (deallocateObject b objects)
                                            let value   | not (isBOOL a) || not (isBOOL b) = ERROR ExpectedBool
                                                        | otherwise = BOOL (getBOOL a && getBOOL b)
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)


funcOR :: StackState
funcOR = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "||"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject a (deallocateObject b objects)
                                            let value   | not (isBOOL a) || not (isBOOL b) = ERROR ExpectedBool
                                                        | otherwise = BOOL (getBOOL a || getBOOL b)
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)


funcNOT :: StackState
funcNOT = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "not"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let newObjects = deallocateObject a objects
                                            let value   | not (isBOOL a) = ERROR ExpectedBool
                                                        | otherwise = BOOL (not $ getBOOL a)
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)
