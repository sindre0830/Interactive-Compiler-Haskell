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

executeStack :: Stack -> StackState
executeStack [] = do
    (objects, variables, stack) <- get
    return (objects, reverse stack)
executeStack (x:xs) = do
    if isFUNC x
        then (
            case getFUNC x of
                -- Arithmetic
                "+" -> funcAddition
                "-" -> funcSubtraction
                -- Bool
                "&&" -> funcAND
                -- Comparison
                "==" -> funcEqual
                -- Stack
                -- String
                -- List
                "empty" -> funcEmpty
                "head" -> funcHead
                "tail" -> funcTail
                "cons" -> funcCons
                "append" -> funcAppend
                -- Length
                -- Code block
                -- Control flow
            ) >> executeStack xs
        else do
            (objects, variables, stack) <- get
            put (objects, variables, x : stack)
            executeStack xs

{- Arithmetic -}

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
                                                                    | isFLOAT a && isINT b = (FLOAT (getFLOAT a + fromIntegral (getINT b)) : rest, objects)
                                                                    | isINT a && isINT b = (INT (getINT a + getINT b) : rest, objects)
                                                                    | isFLOAT a && isFLOAT b = (FLOAT (getFLOAT a + getFLOAT b) : rest, objects)
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcSubtraction :: StackState
funcSubtraction = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "-"
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
                                                                    | isINT a && isFLOAT b = (FLOAT (fromIntegral (getINT a) - getFLOAT b) : rest, objects)
                                                                    | isFLOAT a && isINT b = (FLOAT (getFLOAT a - fromIntegral (getINT b)) : rest, objects)
                                                                    | isINT a && isINT b = (INT (getINT a - getINT b) : rest, objects)
                                                                    | isFLOAT a && isFLOAT b = (FLOAT (getFLOAT a - getFLOAT b) : rest, objects)
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Bool -}

funcAND :: StackState
funcAND = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "&&"
                                        then ([ERROR InvalidParameterAmount], objects)
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject b objects
                                        let objects = deallocateObject a newObjects
                                        if not (isBOOL a) || not (isBOOL b)
                                            then (ERROR ExpectedBool : rest, newObjects)
                                        else (BOOL (getBOOL a && getBOOL b) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Comparison -}

funcEqual :: StackState
funcEqual = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "=="
                                        then ([ERROR InvalidParameterAmount], objects)
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject b objects
                                        let objects = deallocateObject a newObjects
                                        (BOOL (a == b) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Stack -}

{- String -}

{- List -}

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
    return (newObjects, reverse newStack)

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
                                        else do
                                            let list = objects Map.! getLIST a
                                            if null list
                                                then (rest, newObjects)
                                            else (head list : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

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
                                            let list = objects Map.! key
                                            let newObjects =   (if null list
                                                                    then objects
                                                                else updateObject key (tail list) objects)
                                            (stack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

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
                                        if not $ isLIST b
                                            then do
                                                let newObjects = deallocateObject b objects
                                                let objects = deallocateObject a newObjects
                                                (ERROR ExpectedList : rest, objects)
                                        else do
                                            let key = getLIST b
                                            let newObjects = updateObject key (a : (objects Map.! key)) objects
                                            (b : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

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
                                            let newObjects = updateObject keyB ((objects Map.! keyA) ++ (objects Map.! keyB)) objects
                                            let objects = deallocateObject a newObjects
                                            (b : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Length -}

{- Code block -}

{- Control flow -}
