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
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    if null inpStack || statusIO /= None
        then do
            put (inpStack, objects, variables, functions, outStack, statusIO)
            return (inpStack, objects, variables, functions, outStack, statusIO)
    else do
        let (x:xs) = inpStack
        let (shouldSkip, newInpStack, newOutStack) = skipOperation inpStack variables functions
        if shouldSkip
            then do
                put (newInpStack, objects, variables, functions, newOutStack ++ outStack, statusIO)
                executeStack
        else if isFUNC x
            then ( do
                put (xs, objects, variables, functions, outStack, statusIO)
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
                    "loop"  -> funcLoop
                    -- Assignment
                    ":="    -> funcSetVariable
                    "fun"   -> funcSetFunction
                    -- IO
                    "read"  -> funcRead
                    "print" -> funcPrint
                ) >> executeStack
        else do
            let (moveToBuffer, newObjects, newOutStack) = setVariable [x] variables functions (False, objects, [])
            if moveToBuffer
                then put (reverse newOutStack ++ xs, newObjects, variables, functions, outStack, statusIO) >> executeStack
            else put (xs, newObjects, variables, functions, newOutStack ++ outStack, statusIO) >> executeStack

setVariable :: Stack -> Variables -> Functions -> (Bool, Objects, OutputStack) -> (Bool, Objects, OutputStack)
setVariable [] _ _ (moveToBuffer, objects, outStack) = (moveToBuffer, objects, outStack)
setVariable (x:xs) variables functions (moveToBuffer, objects, outStack)
    | isLIST x = do
        let key = getLIST x
        let list = objects Map.! key
        let (newMoveToBuffer, newObjects, newOutStack) = setVariable list variables functions (moveToBuffer, objects, [])
        let objects = updateObject key (reverse newOutStack) newObjects
        setVariable xs variables functions (newMoveToBuffer, objects, x : outStack)
    | isCODEBLOCK x = do
        let key = getCODEBLOCK x
        let block = objects Map.! key
        let (newMoveToBuffer, newObjects, newOutStack) = setVariable block variables functions (moveToBuffer, objects, [])
        let objects = updateObject key (reverse newOutStack) newObjects
        setVariable xs variables functions (newMoveToBuffer, objects, x : outStack)
    | isUNKNOWN x && Map.member (getUNKNOWN x) variables = do
        let value = variables Map.! getUNKNOWN x
        let (newStack, newObjects) = duplicateStack [value] ([], objects)
        setVariable xs variables functions (moveToBuffer, newObjects, newStack ++ outStack)
    | isUNKNOWN x && Map.member (getUNKNOWN x) functions = do
        let value = functions Map.! getUNKNOWN x
        let (newStack, newObjects) = duplicateStack value ([], objects)
        setVariable xs variables functions (True, newObjects, reverse newStack ++ outStack)
    | otherwise = setVariable xs variables functions (moveToBuffer, objects, x : outStack)

skipOperation :: Stack -> Variables -> Functions -> (Bool, Stack, Stack)
skipOperation stack variables functions
    | length stack >= 3 && isFUNC (stack !! 2) 
        && (getFUNC (stack !! 2) == "loop"
        || getFUNC (stack !! 2) == "if") = do
            let (x:y:rest) = stack
            (True, rest, y:[x])
    | length stack >= 2 && isFUNC (stack !! 1) 
        && (getFUNC (stack !! 1) == "map"
        || getFUNC (stack !! 1) == "each"
        || getFUNC (stack !! 1) == "loop"
        || getFUNC (stack !! 1) == "times"
        || getFUNC (stack !! 1) == "if"
        || getFUNC (stack !! 1) == "foldl") = do
            let (x:rest) = stack
            (True, rest, [x])
    | length stack >= 3 && isFUNC (stack !! 2) && isUNKNOWN (head stack) 
        && (Map.member (getUNKNOWN (head stack)) variables || Map.member (getUNKNOWN (head stack)) functions)
        && (getFUNC (stack !! 2) == ":="
        || getFUNC (stack !! 2) == "fun") = do
            let (x:y:rest) = stack
            (True, rest, y:[x])
    | otherwise = (False, stack, [])

{- Arithmetic -}

funcAddition :: StackState
funcAddition = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "+"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject b (deallocateObject a objects)
                                            let value   | isINT a && isINT b        = INT      (getINT a       + getINT b)
                                                        | isINT a && isFLOAT b      = FLOAT    (convertFloat a + getFLOAT b)
                                                        | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     + convertFloat b)
                                                        | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     + getFLOAT b)
                                                        | otherwise = ERROR ExpectedNumber
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcSubtraction :: StackState
funcSubtraction = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "-"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject b (deallocateObject a objects)
                                            let value   | isINT a && isINT b        = INT      (getINT a       - getINT b)
                                                        | isINT a && isFLOAT b      = FLOAT    (convertFloat a - getFLOAT b)
                                                        | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     - convertFloat b)
                                                        | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     - getFLOAT b)
                                                        | otherwise = ERROR ExpectedNumber
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcMultiplication :: StackState
funcMultiplication = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "*"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject b (deallocateObject a objects)
                                            let value   | isINT a && isINT b        = INT      (getINT a       * getINT b)
                                                        | isINT a && isFLOAT b      = FLOAT    (convertFloat a * getFLOAT b)
                                                        | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     * convertFloat b)
                                                        | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     * getFLOAT b)
                                                        | otherwise = ERROR ExpectedNumber
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcDivisionFloat :: StackState
funcDivisionFloat = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "/"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject b (deallocateObject a objects)
                                            let value   | isINT a && isINT b        = FLOAT    (convertFloat a / convertFloat b)
                                                        | isINT a && isFLOAT b      = FLOAT    (convertFloat a / getFLOAT b)
                                                        | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     / convertFloat b)
                                                        | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     / getFLOAT b)
                                                        | otherwise = ERROR ExpectedNumber
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcDivisionInteger :: StackState
funcDivisionInteger = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "div"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject b (deallocateObject a objects)
                                            let value   | isINT a && isINT b        = INT           (getINT a   `div` getINT b)
                                                        | isINT a && isFLOAT b      = INT   (floor  (convertFloat a / getFLOAT b))
                                                        | isFLOAT a && isINT b      = INT   (floor  (getFLOAT a     / convertFloat b))
                                                        | isFLOAT a && isFLOAT b    = INT   (floor  (getFLOAT a     / getFLOAT b))
                                                        | otherwise = ERROR ExpectedNumber
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

{- Bool -}

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

{- Comparison -}

funcEqual :: StackState
funcEqual = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "=="
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject a (deallocateObject b objects)
                                            let value = compareStack [a] [b] objects
                                            (BOOL value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

compareStack :: Stack -> Stack -> Objects -> Bool
compareStack [] [] _ = True
compareStack [] _ _ = False
compareStack _ [] _ = False
compareStack (x:xs) (y:ys) objects
    | isLIST x && isLIST y = compareStack (objects Map.! getLIST x) (objects Map.! getLIST y) objects
    | isCODEBLOCK x && isCODEBLOCK y = compareStack (objects Map.! getCODEBLOCK x) (objects Map.! getCODEBLOCK y) objects
    | isINT x && isFLOAT y = convertFloat x == getFLOAT y
    | isFLOAT x && isINT y = getFLOAT x == convertFloat y
    | x /= y = False
    | otherwise = compareStack xs ys objects

funcLess :: StackState
funcLess = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "<"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject a (deallocateObject b objects)
                                            let value   | isINT a && isINT b        = BOOL  (getINT a       < getINT b)
                                                        | isINT a && isFLOAT b      = BOOL  (convertFloat a < getFLOAT b)
                                                        | isFLOAT a && isINT b      = BOOL  (getFLOAT a     < convertFloat b)
                                                        | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     < getFLOAT b)
                                                        | otherwise = ERROR ExpectedNumber
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcGreater :: StackState
funcGreater = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! ">"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            let newObjects = deallocateObject a (deallocateObject b objects)
                                            let value   | isINT a && isINT b        = BOOL  (getINT a       > getINT b)
                                                        | isINT a && isFLOAT b      = BOOL  (convertFloat a > getFLOAT b)
                                                        | isFLOAT a && isINT b      = BOOL  (getFLOAT a     > convertFloat b)
                                                        | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     > getFLOAT b)
                                                        | otherwise = ERROR ExpectedNumber
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

{- Stack -}

funcPop :: StackState
funcPop = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "pop"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let newObjects = deallocateObject a objects
                                            (rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcDup :: StackState
funcDup = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "dup"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let (value, newObjects) = duplicateStack [a] ([], objects)
                                            (head value : outStack, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcSwap :: StackState
funcSwap = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "swap"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            (a:b:rest, objects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

{- String -}

funcParseInteger :: StackState
funcParseInteger = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "parseInteger"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let newObjects = deallocateObject a objects
                                            let value   | not (isSTRING a) = ERROR ExpectedString
                                                        | isJust (readMaybe (getSTRING a) :: Maybe Integer) = INT (fromJust (readMaybe (getSTRING a) :: Maybe Integer))
                                                        | otherwise = ERROR ExpectedStringOfInteger
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcParseFloat :: StackState
funcParseFloat = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "parseFloat"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let newObjects = deallocateObject a objects
                                            let value   | not (isSTRING a) = ERROR ExpectedString
                                                        | isJust (readMaybe (getSTRING a) :: Maybe Float) = FLOAT (fromJust (readMaybe (getSTRING a) :: Maybe Float))
                                                        | otherwise = ERROR ExpectedStringOfFloat
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcWords :: StackState
funcWords = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "words"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let (value, newObjects) | not (isSTRING a) = (ERROR ExpectedString, deallocateObject a objects)
                                                                    | otherwise = do
                                                                        let list = map STRING (words $ getSTRING a)
                                                                        let (newObjects, key) = allocateObject list objects
                                                                        (LIST key, newObjects)
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

{- List -}

funcEmpty :: StackState
funcEmpty = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "empty"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let newObjects = deallocateObject a objects
                                            let value   | not (isLIST a) = ERROR ExpectedList
                                                        | otherwise = BOOL (null (objects Map.! getLIST a))
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcHead :: StackState
funcHead = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "head"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let newObjects = deallocateObject a objects
                                            if not (isLIST a)
                                                then (ERROR ExpectedList : rest, newObjects)
                                            else do
                                                let list = objects Map.! getLIST a
                                                if null list
                                                    then (rest, newObjects)
                                                else (head list : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcTail :: StackState
funcTail = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "tail"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            if not (isLIST a)
                                                then do
                                                    let newObjects = deallocateObject a objects
                                                    (ERROR ExpectedList : rest, newObjects)
                                            else do
                                                let key = getLIST a
                                                let list = objects Map.! key
                                                let newObjects  | null list = objects
                                                                | otherwise = updateObject key (tail list) objects
                                                (outStack, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcCons :: StackState
funcCons = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "cons"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            if not (isLIST b)
                                                then do
                                                    let newObjects = deallocateObject a (deallocateObject b newObjects)
                                                    (ERROR ExpectedList : rest, newObjects)
                                            else do
                                                let key = getLIST b
                                                let newObjects = updateObject key (a : (objects Map.! key)) objects
                                                (b : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

funcAppend :: StackState
funcAppend = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "append"
                                            then deallocateStack outStack objects
                                        else do
                                            let (b:a:rest) = outStack
                                            if not (isLIST a) || not (isLIST b)
                                                then do
                                                    let newObjects = deallocateObject a (deallocateObject b newObjects)
                                                    (ERROR ExpectedList : rest, newObjects)
                                            else do
                                                let keyA = getLIST a
                                                let keyB = getLIST b
                                                let newObjects = deallocateObject a (updateObject keyB ((objects Map.! keyA) ++ (objects Map.! keyB)) objects)
                                                (b : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

{- Length -}

funcLength :: StackState
funcLength = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = (   if length outStack < functors Map.! "length"
                                            then deallocateStack outStack objects
                                        else do
                                            let (a:rest) = outStack
                                            let newObjects = deallocateObject a objects
                                            let value   | isSTRING a = INT (toInteger $ length $ getSTRING a)
                                                        | isLIST a = INT (toInteger $ length $ objects Map.! getLIST a)
                                                        | isCODEBLOCK a = INT (toInteger $ length $ objects Map.! getCODEBLOCK a)
                                                        | otherwise = ERROR ExpectedList
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

{- Code block -}

funcExec :: StackState
funcExec = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newOutStack, newObjects) = (  if length outStack < functors Map.! "exec"
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (inpStack, newOutStack, newObjects)
                                                    else do
                                                        let (a:rest) = outStack
                                                        if not (isCODEBLOCK a)
                                                            then (inpStack, ERROR ExpectedCodeblock : rest, deallocateObject a objects)
                                                        else do
                                                            let block = objects Map.! getCODEBLOCK a
                                                            (block ++ inpStack, rest, deallocateOneObject a objects)
                                                )
    put (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (newInpStack, newObjects, variables, functions, newOutStack, statusIO)

{- Control flow -}

funcIf :: StackState
funcIf = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newOutStack, newObjects) = (  if length outStack < functors Map.! "if"
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (inpStack, newOutStack, newObjects)
                                                    else do
                                                        let (c:b:a:rest) = outStack
                                                        let newObjects = deallocateObject a (deallocateObject b (deallocateObject c objects))
                                                        if not (isBOOL a)
                                                            then (inpStack, ERROR ExpectedBool : rest, newObjects)
                                                        else if getBOOL a
                                                            then do
                                                                let values  | isCODEBLOCK b = [b, FUNC "exec"]
                                                                            | otherwise = [b]
                                                                (values ++ inpStack, rest, deallocateObject a (deallocateObject c objects))
                                                        else do
                                                            let values  | isCODEBLOCK c = [c, FUNC "exec"]
                                                                        | otherwise = [c]
                                                            (values ++ inpStack, rest, deallocateObject a (deallocateObject b objects))
                                                )
    put (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (newInpStack, newObjects, variables, functions, newOutStack, statusIO)

funcMap :: StackState
funcMap = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newVariables, newFunctions, newOutStack) = (   if length outStack < functors Map.! "map"
                                                                        then do
                                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                                            (newObjects, variables, functions, newOutStack)
                                                                    else do
                                                                        let (b:a:rest) = outStack
                                                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                                                        if not (isLIST a)
                                                                            then (newObjects, variables, functions, ERROR ExpectedList : rest)
                                                                        else if not (isCODEBLOCK b) && not (isFUNC b) && not (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                                                                            then (newObjects, variables, functions, ERROR ExpectedCodeblock : rest)
                                                                        else do
                                                                            let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                                                                        | otherwise = [b]
                                                                            let list = objects Map.! getLIST a
                                                                            let (newObjects, newVariables, newFunctions, newList) = mapOf list block (objects, variables, functions, [])
                                                                            let (_, objects) = deallocateStack block newObjects
                                                                            if head newList == ERROR InvalidOperationIO
                                                                                then do
                                                                                    (deallocateObject a objects, variables, functions, head newList : rest)
                                                                            else do
                                                                                let newObjects = updateObject (getLIST a) (reverse newList) objects
                                                                                (newObjects, newVariables, newFunctions, a : rest)
                                                                )
    put (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)
    return (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)

funcEach :: StackState
funcEach = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newVariables, newFunctions, newOutStack) = (   if length outStack < functors Map.! "each"
                                                                        then do
                                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                                            (newObjects, variables, functions, newOutStack)
                                                                    else do
                                                                        let (b:a:rest) = outStack
                                                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                                                        if not (isLIST a)
                                                                            then (newObjects, variables, functions, ERROR ExpectedList : rest)
                                                                        else if not (isCODEBLOCK b) && not (isFUNC b) && not (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                                                                            then (newObjects, variables, functions, ERROR ExpectedCodeblock : rest)
                                                                        else do
                                                                            let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                                                                        | otherwise = [b]
                                                                            let list = objects Map.! getLIST a
                                                                            let (newObjects, newVariables, newFunctions, values) = mapOf list block (objects, variables, functions, [])
                                                                            let (_, objects) = deallocateStack block newObjects
                                                                            (deallocateObject a objects, newVariables, newFunctions, values ++ rest)
                                                                )
    put (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)
    return (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)

mapOf :: Stack -> Stack -> (Objects, Variables, Functions, OutputStack) -> (Objects, Variables, Functions, OutputStack)
mapOf [] _ (objects, variables, functions, outStack) = (objects, variables, functions, outStack)
mapOf (x:xs) block (objects, variables, functions, outStack) = do
    let (dupBlock, newObjects) = duplicateStack block ([], objects)
    let (_, objects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newObjects, variables, functions, [x], None)
    if statusIO /= None
        then do
            let (_, newObjects) = deallocateStack outStack objects
            (newObjects, variables, functions, [ERROR InvalidOperationIO])
    else mapOf xs block (objects, newVariables, newFunctions, newOutStack ++ outStack)

funcFoldl :: StackState
funcFoldl = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newVariables, newFunctions, newOutStack) = (   if length outStack < functors Map.! "foldl"
                                                                        then do
                                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                                            (newObjects, variables, functions, newOutStack)
                                                                    else do
                                                                        let (c:b:a:rest) = outStack
                                                                        let newObjects = deallocateObject a (deallocateObject b (deallocateObject c objects))
                                                                        if not (isLIST a)
                                                                            then (newObjects, variables, functions, ERROR ExpectedList : rest)
                                                                        else if isCODEBLOCK b || isFUNC b  || (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                                                                            then (newObjects, variables, functions, ERROR InvalidType : rest)
                                                                        else if not (isCODEBLOCK c) && not (isFUNC c) && not (isUNKNOWN c && Map.member (getUNKNOWN c) functions)
                                                                            then (newObjects, variables, functions, ERROR ExpectedCodeblock : rest)
                                                                        else do
                                                                            let block   | isCODEBLOCK c = [c, FUNC "exec"]
                                                                                        | otherwise = [c]
                                                                            let list = objects Map.! getLIST a
                                                                            let (newObjects, newVariables, newFunctions, newValue) = foldlOf list block (objects, variables, functions, b)
                                                                            let (_, objects) = deallocateStack block newObjects
                                                                            (deallocateObject a objects, newVariables, newFunctions, newValue : rest)
                                                                )
    put (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)
    return (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)

foldlOf :: Stack -> Stack -> (Objects, Variables, Functions, Type) -> (Objects, Variables, Functions, Type)
foldlOf [] _ (objects, variables, functions, value) = (objects, variables, functions, value)
foldlOf (x:xs) block (objects, variables, functions, value) = do
    let (dupBlock, newObjects) = duplicateStack block ([], objects)
    let (_, objects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newObjects, variables, functions, x : [value], None)
    if statusIO /= None
        then do
            let (_, objects) = deallocateStack dupBlock newObjects
            (deallocateObject value newObjects, variables, functions, ERROR InvalidOperationIO)
    else foldlOf xs block (objects, newVariables, newFunctions, head newOutStack)

funcTimes :: StackState
funcTimes = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newObjects, newOutStack) = (  if length outStack < functors Map.! "times"
                                                        then do
                                                            let (newStack, newObjects) = deallocateStack outStack objects
                                                            (inpStack, newObjects, newStack)
                                                    else do
                                                        let (b:a:rest) = outStack
                                                        let newObject = deallocateObject a (deallocateObject b objects)
                                                        if not (isINT a) || getINT a < 0
                                                            then (inpStack, newObject, ERROR ExpectedPositiveInteger : rest)
                                                        else do
                                                            let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                                                        | otherwise = [b]
                                                            let (values, newObjects) = loopN (getINT a) block ([], objects)
                                                            let (_, objects) = deallocateStack block newObjects
                                                            (values ++ inpStack, deallocateObject a objects, rest)
                                                )
    put (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (newInpStack, newObjects, variables, functions, newOutStack, statusIO)

loopN :: Integer -> Stack -> (Stack, Objects) -> (Stack, Objects)
loopN 0 _ (stack, objects) = (stack, objects)
loopN n block (stack, objects) = do
    let (dupBlock, newObjects) = duplicateStack block ([], objects)
    loopN (n - 1) block (dupBlock ++ stack, newObjects)

funcLoop :: StackState
funcLoop = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newInpStack, newObjects, newOutStack) = (  if length outStack < functors Map.! "loop"
                                                        then do
                                                            let (newStack, newObjects) = deallocateStack outStack objects
                                                            (inpStack, newObjects, newStack)
                                                    else do
                                                        let (b:a:rest) = outStack
                                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                                        if not (isCODEBLOCK a) && not (isFUNC a)  && not (isUNKNOWN a && Map.member (getUNKNOWN a) functions) 
                                                            || not (isCODEBLOCK b) && not (isFUNC b) && not (isUNKNOWN b && Map.member (getUNKNOWN b) functions)
                                                            then (inpStack, newObjects, ERROR ExpectedCodeblock : rest)
                                                        else do
                                                            let break   | isCODEBLOCK a = [a, FUNC "exec"]
                                                                        | otherwise = [a]
                                                            let block   | isCODEBLOCK b = [b, FUNC "exec"]
                                                                        | otherwise = [b]
                                                            let (newObjects, values) = loop break block (objects, variables, functions, rest)
                                                            let (_, objects) = deallocateStack (break ++ block) newObjects
                                                            (values ++ inpStack, objects, [])
                                                )
    put (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (newInpStack, newObjects, variables, functions, newOutStack, statusIO)

loop :: Stack -> Stack -> (Objects, Variables, Functions, Stack) -> (Objects, Stack)
loop break block (objects, variables, functions, outStack) = do
    let (_, _, _, _, newOutStack, statusIO) = evalState executeStack (break, objects, variables, functions, outStack, None)
    let value = head newOutStack
    if statusIO /= None
        then (objects, [ERROR InvalidOperationIO])
    else if not (isBOOL value)
        then (objects, [ERROR ExpectedBool])
    else if getBOOL value
        then (objects, reverse outStack)
    else do
        let (dupBlock, newObjects) = duplicateStack block ([], objects)
        let (_, objects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (dupBlock, newObjects, variables, functions, outStack, None)
        if statusIO /= None
            then (objects, [ERROR InvalidOperationIO])
        else loop break block (objects, newVariables, newFunctions, newOutStack)

funcSetVariable :: StackState
funcSetVariable = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newVariables, newOutStack) = ( if length outStack < functors Map.! ":="
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (newObjects, variables, newOutStack)
                                                    else do
                                                        let (b:a:rest) = outStack
                                                        let newObjects = deallocateObject a objects
                                                        if not (isUNKNOWN a)
                                                            then (deallocateObject b newObjects, variables, ERROR ExpectedUnknown : rest)
                                                        else do
                                                            let newVariables = Map.insert (getUNKNOWN a) b variables
                                                            (newObjects, newVariables, rest)
                                                )
    put (inpStack, newObjects, newVariables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, newVariables, functions, newOutStack, statusIO)

funcSetFunction :: StackState
funcSetFunction = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newFunctions, newOutStack) = ( if length outStack < functors Map.! "fun"
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (newObjects, functions, newOutStack)
                                                    else do
                                                        let (b:a:rest) = outStack
                                                        let newObjects = deallocateObject a (deallocateObject b objects)
                                                        if not (isUNKNOWN a)
                                                            then (newObjects, functions, ERROR ExpectedUnknown : rest)
                                                        else if not (isCODEBLOCK b)
                                                            then (newObjects, functions, ERROR ExpectedCodeblock : rest)
                                                        else do
                                                            let block = objects Map.! getCODEBLOCK b
                                                            let (dupBlock, newObjects) = duplicateStack block ([], objects)
                                                            let newFunctions = Map.insert (getUNKNOWN a) dupBlock functions
                                                            (deallocateObject a (deallocateObject b newObjects), newFunctions, rest)
                                                )
    put (inpStack, newObjects, variables, newFunctions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, newFunctions, newOutStack, statusIO)

{- IO -}

funcRead :: StackState
funcRead = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    put (inpStack, objects, variables, functions, outStack, Input)
    return (inpStack, objects, variables, functions, outStack, Input)

funcPrint :: StackState
funcPrint = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects, newStatusIO) = (  if length outStack < functors Map.! "print"
                                                        then do
                                                            let (newOutStack, newObjects) = deallocateStack outStack objects
                                                            (newOutStack, newObjects, statusIO)
                                                    else do
                                                        let (a:rest) = outStack
                                                        let newObjects = deallocateObject a objects
                                                        if not (isSTRING a)
                                                            then (ERROR ExpectedString : rest, newObjects, statusIO)
                                                        else (PRINT (getSTRING a) : rest, newObjects, Output)
                                                )
    put (inpStack, newObjects, variables, functions, newOutStack, newStatusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, newStatusIO)
