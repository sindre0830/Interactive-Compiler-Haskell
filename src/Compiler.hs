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
                "+"     -> funcAddition
                "-"     -> funcSubtraction
                "*"     -> funcMultiplication
                "/"     -> funcDivisionFloat
                "div"   -> funcDivisionInteger
                -- Bool
                "&&"    -> funcAND
                "||"    -> funcOR
                "not"   -> funcNOT
                -- Comparison
                "=="    -> funcEqual
                "<"     -> funcLess
                ">"     -> funcGreater
                -- Stack
                -- String
                -- List
                "empty"     -> funcEmpty
                "head"      -> funcHead
                "tail"      -> funcTail
                "cons"      -> funcCons
                "append"    -> funcAppend
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
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject b (deallocateObject a objects)
                                        let newStack    | isINT a && isINT b        = INT      (getINT a       + getINT b)         : rest
                                                        | isINT a && isFLOAT b      = FLOAT    (convertFloat a + getFLOAT b)       : rest
                                                        | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     + convertFloat b)   : rest
                                                        | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     + getFLOAT b)       : rest
                                                        | otherwise = ERROR ExpectedNumber : rest
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcSubtraction :: StackState
funcSubtraction = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "-"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject b (deallocateObject a objects)
                                        let newStack    | isINT a && isINT b        = INT      (getINT a       - getINT b)         : rest
                                                        | isINT a && isFLOAT b      = FLOAT    (convertFloat a - getFLOAT b)       : rest
                                                        | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     - convertFloat b)   : rest
                                                        | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     - getFLOAT b)       : rest
                                                        | otherwise = ERROR ExpectedNumber : rest
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcMultiplication :: StackState
funcMultiplication = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "*"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject b (deallocateObject a objects)
                                        let newStack    | isINT a && isINT b        = INT      (getINT a       * getINT b)         : rest
                                                        | isINT a && isFLOAT b      = FLOAT    (convertFloat a * getFLOAT b)       : rest
                                                        | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     * convertFloat b)   : rest
                                                        | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     * getFLOAT b)       : rest
                                                        | otherwise = ERROR ExpectedNumber : rest
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcDivisionFloat :: StackState
funcDivisionFloat = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "/"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject b (deallocateObject a objects)
                                        let newStack    | isINT a && isINT b        = FLOAT    (convertFloat a / convertFloat b)   : rest
                                                        | isINT a && isFLOAT b      = FLOAT    (convertFloat a / getFLOAT b)       : rest
                                                        | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     / convertFloat b)   : rest
                                                        | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     / getFLOAT b)       : rest
                                                        | otherwise = ERROR ExpectedNumber : rest
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcDivisionInteger :: StackState
funcDivisionInteger = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "div"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject b (deallocateObject a objects)
                                        let newStack    | isINT a && isINT b        = INT           (getINT a   `div` getINT b)         : rest
                                                        | isINT a && isFLOAT b      = INT   (floor  (convertFloat a / getFLOAT b))      : rest
                                                        | isFLOAT a && isINT b      = INT   (floor  (getFLOAT a     / convertFloat b))  : rest
                                                        | isFLOAT a && isFLOAT b    = INT   (floor  (getFLOAT a     / getFLOAT b))      : rest
                                                        | otherwise = ERROR ExpectedNumber : rest
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Bool -}

funcAND :: StackState
funcAND = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "&&"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        if not (isBOOL a) || not (isBOOL b)
                                            then (ERROR ExpectedBool : rest, newObjects)
                                        else (BOOL (getBOOL a && getBOOL b) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcOR :: StackState
funcOR = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "||"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        if not (isBOOL a) || not (isBOOL b)
                                            then (ERROR ExpectedBool : rest, newObjects)
                                        else (BOOL (getBOOL a || getBOOL b) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcNOT :: StackState
funcNOT = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "not"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        if not (isBOOL a)
                                            then (ERROR ExpectedBool : rest, newObjects)
                                        else (BOOL (not $ getBOOL a) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Comparison -}

funcEqual :: StackState
funcEqual = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "=="
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        (BOOL (a == b) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcLess :: StackState
funcLess = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "<"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        let newStack    | isINT a && isINT b        = BOOL  (getINT a       < getINT b)         : rest
                                                        | isINT a && isFLOAT b      = BOOL  (convertFloat a < getFLOAT b)       : rest
                                                        | isFLOAT a && isINT b      = BOOL  (getFLOAT a     < convertFloat b)   : rest
                                                        | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     < getFLOAT b)       : rest
                                                        | otherwise = ERROR ExpectedNumber : rest
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcGreater :: StackState
funcGreater = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! ">"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        let newStack    | isINT a && isINT b        = BOOL  (getINT a       > getINT b)         : rest
                                                        | isINT a && isFLOAT b      = BOOL  (convertFloat a > getFLOAT b)       : rest
                                                        | isFLOAT a && isINT b      = BOOL  (getFLOAT a     > convertFloat b)   : rest
                                                        | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     > getFLOAT b)       : rest
                                                        | otherwise = ERROR ExpectedNumber : rest
                                        (newStack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Stack -}

{- String -}

{- List -}

funcEmpty :: StackState
funcEmpty = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "empty"
                                        then deallocateStack stack objects
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
                                        then deallocateStack stack objects
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
                                        then deallocateStack stack objects
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
                                        then deallocateStack stack objects
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
                                        then deallocateStack stack objects
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
