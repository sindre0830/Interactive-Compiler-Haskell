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

executeStack :: StackState
executeStack = do
    (buffer, objects, variables, stack) <- get
    if null buffer
        then do
           put (buffer, objects, variables, stack) 
           return (objects, reverse stack)
    else do
        let (x:xs) = buffer
        if isFUNC x
            then ( do
                put (xs, objects, variables, stack)
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
                    "pop"   -> funcPop
                    "dup"   -> funcDup
                    "swap"  -> funcSwap
                    -- String
                    "parseInteger"  -> funcParseInteger
                    "parseFloat"    -> funcParseFloat
                    "words"         -> funcWords
                    -- List
                    "empty"     -> funcEmpty
                    "head"      -> funcHead
                    "tail"      -> funcTail
                    "cons"      -> funcCons
                    "append"    -> funcAppend
                    -- Length
                    "length"    -> funcLength
                    -- Code block
                    "exec"  -> funcExec
                    -- Control flow
                    "if"    -> funcIf
                    "map"   -> funcMap
                    "each"  -> funcEach
                    "foldl" -> funcFoldl
                    "times" -> funcTimes
                ) >> executeStack
            else do
                put (xs, objects, variables, x : stack)
                executeStack

{- Arithmetic -}

funcAddition :: StackState
funcAddition = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcSubtraction :: StackState
funcSubtraction = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcMultiplication :: StackState
funcMultiplication = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcDivisionFloat :: StackState
funcDivisionFloat = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcDivisionInteger :: StackState
funcDivisionInteger = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Bool -}

funcAND :: StackState
funcAND = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "&&"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        if not (isBOOL a) || not (isBOOL b)
                                            then (ERROR ExpectedBool : rest, newObjects)
                                        else (BOOL (getBOOL a && getBOOL b) : rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcOR :: StackState
funcOR = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "||"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        if not (isBOOL a) || not (isBOOL b)
                                            then (ERROR ExpectedBool : rest, newObjects)
                                        else (BOOL (getBOOL a || getBOOL b) : rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcNOT :: StackState
funcNOT = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "not"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        if not (isBOOL a)
                                            then (ERROR ExpectedBool : rest, newObjects)
                                        else (BOOL (not $ getBOOL a) : rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Comparison -}

funcEqual :: StackState
funcEqual = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "=="
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        let newStack    | isLIST a && isLIST b = BOOL (objects Map.! getLIST a == objects Map.! getLIST b)
                                                        | isCODEBLOCK a && isCODEBLOCK b = BOOL (objects Map.! getCODEBLOCK a == objects Map.! getCODEBLOCK b)
                                                        | otherwise = BOOL (a == b)
                                        (newStack : rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcLess :: StackState
funcLess = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcGreater :: StackState
funcGreater = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Stack -}

funcPop :: StackState
funcPop = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "pop"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        (rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcDup :: StackState
funcDup = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "dup"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let (newStack, newObjects)  | isLIST a = do
                                                                        let object = objects Map.! getLIST a
                                                                        let (newObjects, key) = allocateObject object objects
                                                                        (LIST key : stack, newObjects)
                                                                    | isCODEBLOCK a = do
                                                                        let object = objects Map.! getCODEBLOCK a
                                                                        let (newObjects, key) = allocateObject object objects
                                                                        (CODEBLOCK key : stack, newObjects)
                                                                    | otherwise = (a : stack, objects)
                                        (newStack, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcSwap :: StackState
funcSwap = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "swap"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        (a:b:rest, objects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- String -}

funcParseInteger :: StackState
funcParseInteger = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "parseInteger"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        if not $ isSTRING a
                                            then (ERROR ExpectedString : rest, newObjects)
                                        else if isJust (readMaybe (getSTRING a) :: Maybe Int)
                                            then (INT (fromJust (readMaybe (getSTRING a) :: Maybe Int)) : rest, newObjects)
                                        else (ERROR ExpectedStringOfInteger : rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcParseFloat :: StackState
funcParseFloat = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "parseFloat"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        if not $ isSTRING a
                                            then (ERROR ExpectedString : rest, newObjects)
                                        else if isJust (readMaybe (getSTRING a) :: Maybe Float)
                                            then (FLOAT (fromJust (readMaybe (getSTRING a) :: Maybe Float)) : rest, newObjects)
                                        else (ERROR ExpectedStringOfFloat : rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcWords :: StackState
funcWords = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "words"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        if not $ isSTRING a
                                            then (ERROR ExpectedString : rest, newObjects)
                                        else (map STRING (reverse $ words $ getSTRING a) ++ rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- List -}

funcEmpty :: StackState
funcEmpty = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "empty"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let newObjects = deallocateObject a objects
                                        if not $ isLIST a
                                            then (ERROR ExpectedList : rest, newObjects)
                                        else (BOOL (null (objects Map.! getLIST a)) : rest, newObjects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcHead :: StackState
funcHead = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcTail :: StackState
funcTail = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcCons :: StackState
funcCons = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcAppend :: StackState
funcAppend = do
    (buffer, objects, variables, stack) <- get
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
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Length -}

funcLength :: StackState
funcLength = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "length"
                                        then deallocateStack stack objects
                                    else do
                                        let (a:rest) = stack
                                        let newStack    | isSTRING a = INT (length $ getSTRING a) : rest
                                                        | isLIST a = INT (length $ objects Map.! getLIST a) : rest
                                                        | otherwise = ERROR ExpectedList : rest
                                        (newStack, deallocateObject a objects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Code block -}

funcExec :: StackState
funcExec = do
    (buffer, objects, variables, stack) <- get
    let (newBuffer, newStack, newObjects) =    (if length stack < functors Map.! "exec"
                                                    then do
                                                        let (newStack, newObjects) = deallocateStack stack objects
                                                        (buffer, newStack, newObjects)
                                                else do
                                                    let (a:rest) = stack
                                                    if not $ isCODEBLOCK a
                                                        then (buffer, ERROR ExpectedCodeblock : rest, deallocateObject a objects)
                                                    else do
                                                        let object = objects Map.! getCODEBLOCK a
                                                        (object ++ buffer, rest, deallocateObject a objects))
    put (newBuffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

{- Control flow -}

funcIf :: StackState
funcIf = do
    (buffer, objects, variables, stack) <- get
    let (newBuffer, newStack, newObjects) =    (if length stack < functors Map.! "if"
                                                    then do
                                                        let (newStack, newObjects) = deallocateStack stack objects
                                                        (buffer, newStack, newObjects)
                                                else do
                                                    let (c:b:a:rest) = stack
                                                    let newObjects = deallocateObject a (deallocateObject b (deallocateObject c objects))
                                                    if not $ isBOOL a
                                                        then (buffer, ERROR ExpectedBool : rest, newObjects)
                                                    else if not (isCODEBLOCK b) || not (isCODEBLOCK c)
                                                        then (buffer, ERROR ExpectedCodeblock : rest, newObjects)
                                                    else if getBOOL a
                                                        then do
                                                            let object = objects Map.! getCODEBLOCK b
                                                            (object ++ buffer, rest, newObjects)
                                                    else do
                                                        let object = objects Map.! getCODEBLOCK c
                                                        (object ++ buffer, rest, newObjects))
    put (newBuffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcMap :: StackState
funcMap = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "map"
                                        then deallocateStack stack objects
                                    else do
                                        let (b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                        if not (isLIST a)
                                            then (ERROR ExpectedList : rest, newObjects)
                                        else if not (isCODEBLOCK b)
                                            then (ERROR ExpectedCodeblock : rest, newObjects)
                                        else do
                                            let block = objects Map.! getCODEBLOCK b
                                            let list = objects Map.! getLIST a
                                            let (newList, newObjects) = mapOf list block ([], objects)
                                            let objects = updateObject (getLIST a) newList (deallocateObject b newObjects)
                                            (a : rest, objects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

mapOf :: Stack -> Stack -> (Stack, Object) -> (Stack, Object)
mapOf [] _ (stack, objects) = (stack, objects)
mapOf (x:xs) block (stack, objects) = do
    let (newObjects, newStack) = evalState executeStack (block, objects, Map.empty, [x])
    mapOf xs block (stack ++ newStack, newObjects)

funcEach :: StackState
funcEach = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =    (if length stack < functors Map.! "each"
                                                    then deallocateStack stack objects
                                                else do
                                                    let (b:a:rest) = stack
                                                    let newObjects = deallocateObject a (deallocateObject b objects)
                                                    if not (isLIST a)
                                                        then (ERROR ExpectedList : rest, newObjects)
                                                    else if not (isCODEBLOCK b)
                                                        then (ERROR ExpectedCodeblock : rest, newObjects)
                                                    else do
                                                        let list = objects Map.! getLIST a
                                                        let block = objects Map.! getCODEBLOCK b
                                                        let (newList, objects) = mapOf list block ([], newObjects)
                                                        (newList ++ rest, objects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

funcFoldl :: StackState
funcFoldl = do
    (buffer, objects, variables, stack) <- get
    let (newStack, newObjects) =   (if length stack < functors Map.! "foldl"
                                        then deallocateStack stack objects
                                    else do
                                        let (c:b:a:rest) = stack
                                        let newObjects = deallocateObject a (deallocateObject b (deallocateObject c objects))
                                        if not (isLIST a)
                                            then (ERROR ExpectedList : rest, newObjects)
                                        else if isCODEBLOCK b || isFUNC b
                                            then (ERROR InvalidType : rest, newObjects)
                                        else if not (isCODEBLOCK c)
                                            then (ERROR ExpectedCodeblock : rest, newObjects)
                                        else do
                                            let block = objects Map.! getCODEBLOCK c
                                            if length block /= 1 || not (isFUNC (head block))
                                                then (ERROR ExpectedFunctor : rest, newObjects)
                                            else do
                                                let list = objects Map.! getLIST a
                                                let (newValue, newObjects) = foldlOf list block (b, objects)
                                                let objects = deallocateObject a (deallocateObject c newObjects)
                                                (newValue : rest, objects))
    put (buffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

foldlOf :: Stack -> Stack -> (Type, Object) -> (Type, Object)
foldlOf [] _ (value, objects) = (value, objects)
foldlOf (x:xs) block (value, objects) = do
    let (newObjects, newValue) = evalState executeStack (block, objects, Map.empty, x : [value])
    foldlOf xs block (head newValue, newObjects)

funcTimes :: StackState
funcTimes = do
    (buffer, objects, variables, stack) <- get
    let (newBuffer, newStack, newObjects) =    (if length stack < functors Map.! "times"
                                                    then do
                                                        let (newStack, newObjects) = deallocateStack stack objects
                                                        (buffer, newStack, newObjects)
                                                else do
                                                    let (b:a:rest) = stack
                                                    let newObject = deallocateObject a (deallocateObject b objects)
                                                    if not (isINT a) || getINT a < 0
                                                        then (buffer, ERROR ExpectedPositiveInteger : rest, newObject)
                                                    else if not $ isCODEBLOCK b
                                                        then (buffer, ERROR ExpectedCodeblock : rest, newObject)
                                                    else do
                                                        let block = objects Map.! getCODEBLOCK b
                                                        let newBuffer = loopN (getINT a) block
                                                        (newBuffer ++ buffer, rest, newObject))
    put (newBuffer, newObjects, variables, newStack)
    return (newObjects, reverse newStack)

loopN :: Int -> Stack -> Stack
loopN 0 _ = []
loopN i block = do
    block ++ loopN (i - 1) block
