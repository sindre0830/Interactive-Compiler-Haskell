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

funcAddition :: StackState
funcAddition = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "+"
                                        then do
                                            let newObjects  | not $ null stack = do
                                                                let (a:rest) = stack
                                                                deallocateObject a objects
                                                            | otherwise = objects
                                            ([ERROR InvalidParameterAmount], newObjects)
                                    else do
                                        let (b:a:rest) = stack
                                        let (newStack, newObjects)  | not (isINT a || isFLOAT a) || not (isINT b || isFLOAT b) = do
                                                                        let newObjects = deallocateObject a objects
                                                                        (ERROR ExpectedNumber : rest, deallocateObject b newObjects)
                                                                    | isINT a && isFLOAT b = (FLOAT (fromIntegral (getINT a) + getFLOAT b) : rest, objects)
                                                                    | isINT b && isFLOAT a = (FLOAT (fromIntegral (getINT b) + getFLOAT a) : rest, objects)
                                                                    | isINT a && isINT b = (INT (getINT a + getINT b) : rest, objects)
                                                                    | isFLOAT a && isFLOAT b = (FLOAT (getFLOAT a + getFLOAT b) : rest, objects)
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, newStack)

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

funcAppend :: StackState
funcAppend = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "append"
                                        then do
                                            let newObjects  | not $ null stack = do
                                                                let (a:rest) = stack
                                                                deallocateObject a objects
                                                            | otherwise = objects
                                            ([ERROR InvalidParameterAmount], newObjects)
                                    else do
                                        let (b:a:rest) = stack
                                        if not (isLIST a) || not (isLIST b)
                                            then do
                                                let newObjects = deallocateObject a objects
                                                let objects = deallocateObject b newObjects
                                                (ERROR ExpectedList : rest, objects)
                                        else do
                                            let keyA = getLIST a
                                            let keyB = getLIST b
                                            let newObjects = updateObject keyA ((objects Map.! keyB) ++ (objects Map.! keyA)) objects
                                            let objects = deallocateObject b newObjects
                                            (a : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, newStack)
