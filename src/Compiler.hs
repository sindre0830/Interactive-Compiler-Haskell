module Compiler
    ( module Compiler
    ) where
-- foreign modules
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isJust )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
-- local modules
import Dictionary
import Stack

funcEmpty :: StackState
funcEmpty = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "empty"
                                        then ([ERROR InvalidParameterAmount], objects)
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        if not $ isLIST a
                                            then (ERROR ExpectedList : rest, newObjects)
                                        else (BOOL (null (objects Map.! getLIST a)) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, newStack)

funcHead :: StackState
funcHead = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "head"
                                        then ([ERROR InvalidParameterAmount], objects)
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        if not $ isLIST a
                                            then (ERROR ExpectedList : rest, newObjects)
                                        else (head (objects Map.! getLIST a) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, newStack)

funcTail :: StackState
funcTail = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "tail"
                                        then ([ERROR InvalidParameterAmount], objects)
                                    else do
                                        let (a:rest) = stack
                                        if not $ isLIST a
                                            then do
                                                let newObjects = deallocateObject a objects
                                                (ERROR ExpectedList : rest, newObjects)
                                        else do
                                            let key = getLIST a
                                            let newObjects = updateObject key (tail (objects Map.! key)) objects
                                            (stack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, newStack)

funcCons :: StackState
funcCons = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "cons"
                                        then do
                                            let newObjects  | not $ null stack = do
                                                                let (a:rest) = stack
                                                                deallocateObject a objects
                                                            | otherwise = objects
                                            ([ERROR InvalidParameterAmount], newObjects)
                                    else do
                                        let (b:a:rest) = stack
                                        if not $ isLIST a
                                            then do
                                                let newObjects = deallocateObject a objects
                                                let objects = deallocateObject b newObjects
                                                (ERROR ExpectedList : rest, objects)
                                        else do
                                            let key = getLIST a
                                            let newObjects = updateObject key (b : (objects Map.! key)) objects
                                            (a : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, newStack)
