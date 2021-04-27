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
        if isFUNC x
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
        else if isUNKNOWN x && Map.member (getUNKNOWN x) variables
            then do
                let value = variables Map.! getUNKNOWN x
                let (newValue, newObjects) = duplicateStack [value] ([], objects)
                put (xs, newObjects, variables, functions, head newValue : outStack, statusIO)
                executeStack 
        else if isUNKNOWN x && Map.member (getUNKNOWN x) functions
            then do
                let value = functions Map.! getUNKNOWN x
                let (newValue, newObjects) = duplicateStack value ([], objects)
                put (newValue ++ xs, newObjects, variables, functions, outStack, statusIO)
                executeStack 
        else do
            put (xs, objects, variables, functions, x : outStack, statusIO)
            executeStack

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
                                            let value   | isLIST a && isLIST b = BOOL (objects Map.! getLIST a == objects Map.! getLIST b)
                                                        | isCODEBLOCK a && isCODEBLOCK b = BOOL (objects Map.! getCODEBLOCK a == objects Map.! getCODEBLOCK b)
                                                        | otherwise = BOOL (a == b)
                                            (value : rest, newObjects)
                                    )
    put (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (inpStack, newObjects, variables, functions, newOutStack, statusIO)

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
                                                        | isJust (readMaybe (getSTRING a) :: Maybe Int) = INT (fromJust (readMaybe (getSTRING a) :: Maybe Int))
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
                                            let newObjects = deallocateObject a objects
                                            let values  | not (isSTRING a) = [ERROR ExpectedString]
                                                        | otherwise = map STRING (reverse $ words $ getSTRING a)
                                            (values ++ rest, newObjects)
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
                                            let value   | isSTRING a = INT (length $ getSTRING a)
                                                        | isLIST a = INT (length $ objects Map.! getLIST a)
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
                                                        let newObjects = deallocateObject a objects
                                                        if not (isCODEBLOCK a)
                                                            then (inpStack, ERROR ExpectedCodeblock : rest, newObjects)
                                                        else do
                                                            let block = objects Map.! getCODEBLOCK a
                                                            (block ++ inpStack, rest, newObjects)
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
                                                        else if not (isCODEBLOCK b) || not (isCODEBLOCK c)
                                                            then (inpStack, ERROR ExpectedCodeblock : rest, newObjects)
                                                        else if getBOOL a
                                                            then do
                                                                let block = objects Map.! getCODEBLOCK b
                                                                (block ++ inpStack, rest, newObjects)
                                                        else do
                                                            let block = objects Map.! getCODEBLOCK c
                                                            (block ++ inpStack, rest, newObjects)
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
                                                                        else if not (isCODEBLOCK b)
                                                                            then (newObjects, variables, functions, ERROR ExpectedCodeblock : rest)
                                                                        else do
                                                                            let block = objects Map.! getCODEBLOCK b
                                                                            let list = objects Map.! getLIST a
                                                                            let (newObjects, newVariables, newFunctions, newList) = mapOf list block (objects, variables, functions, [])
                                                                            if head newList == ERROR InvalidOperationIO
                                                                                then (deallocateObject a (deallocateObject b newObjects), newVariables, newFunctions, head newList : rest)
                                                                            else do
                                                                                let objects = updateObject (getLIST a) (reverse newList) (deallocateObject b newObjects)
                                                                                (objects, newVariables, newFunctions, a : rest)
                                                                )
    put (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)
    return (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)

mapOf :: Stack -> Stack -> (Objects, Variables, Functions, OutputStack) -> (Objects, Variables, Functions, OutputStack)
mapOf [] _ (objects, variables, functions, outStack) = (objects, variables, functions, outStack)
mapOf (x:xs) block (objects, variables, functions, outStack) = do
    let (_, newObjects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (block, objects, variables, functions, [x], None)
    if statusIO /= None
        then do
            let (_, newObjects) = deallocateStack outStack objects
            (newObjects, variables, functions, [ERROR InvalidOperationIO])
    else mapOf xs block (newObjects, newVariables, newFunctions, newOutStack ++ outStack)

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
                                                                        else if not (isCODEBLOCK b)
                                                                            then (newObjects, variables, functions, ERROR ExpectedCodeblock : rest)
                                                                        else do
                                                                            let list = objects Map.! getLIST a
                                                                            let block = objects Map.! getCODEBLOCK b
                                                                            let (objects, newVariables, newFunctions, values) = mapOf list block (newObjects, variables, functions, [])
                                                                            (objects, newVariables, newFunctions, values ++ rest)
                                                                )
    put (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)
    return (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)

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
                                                                        else if isCODEBLOCK b || isFUNC b
                                                                            then (newObjects, variables, functions, ERROR InvalidType : rest)
                                                                        else if not (isCODEBLOCK c)
                                                                            then (newObjects, variables, functions, ERROR ExpectedCodeblock : rest)
                                                                        else do
                                                                            let block = objects Map.! getCODEBLOCK c
                                                                            if length block /= 1 || not (isFUNC (head block))
                                                                                then (newObjects, variables, functions, ERROR ExpectedFunctor : rest)
                                                                            else do
                                                                                let list = objects Map.! getLIST a
                                                                                let (newObjects, newVariables, newFunctions, newValue) = foldlOf list block (objects, variables, functions, b)
                                                                                let objects = deallocateObject a (deallocateObject c newObjects)
                                                                                (objects, newVariables, newFunctions, newValue : rest)
                                                                )
    put (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)
    return (inpStack, newObjects, newVariables, newFunctions, newOutStack, statusIO)

foldlOf :: Stack -> Stack -> (Objects, Variables, Functions, Type) -> (Objects, Variables, Functions, Type)
foldlOf [] _ (objects, variables, functions, value) = (objects, variables, functions, value)
foldlOf (x:xs) block (objects, variables, functions, value) = do
    let (_, newObjects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (block, objects, variables, functions, x : [value], None)
    if statusIO /= None
        then (deallocateObject value objects, variables, functions, ERROR InvalidOperationIO)
    else foldlOf xs block (newObjects, newVariables, newFunctions, head newOutStack)

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
                                                        else if not $ isCODEBLOCK b
                                                            then (inpStack, newObject, ERROR ExpectedCodeblock : rest)
                                                        else do
                                                            let block = objects Map.! getCODEBLOCK b
                                                            let values = loopN (getINT a) block
                                                            (values ++ inpStack, newObject, rest)
                                                )
    put (newInpStack, newObjects, variables, functions, newOutStack, statusIO)
    return (newInpStack, newObjects, variables, functions, newOutStack, statusIO)

loopN :: Int -> Stack -> Stack
loopN 0 _ = []
loopN n block = block ++ loopN (n - 1) block

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
                                                        if not (isCODEBLOCK a) || not (isCODEBLOCK b)
                                                            then (inpStack, newObjects, ERROR ExpectedCodeblock : rest)
                                                        else do
                                                            let break = objects Map.! getCODEBLOCK a
                                                            let block = objects Map.! getCODEBLOCK b
                                                            let (objects, values) = loop break block (newObjects, variables, functions, rest)
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
        let (_, newObjects, newVariables, newFunctions, newOutStack, statusIO) = evalState executeStack (block, objects, variables, functions, outStack, None)
        if statusIO /= None
            then (objects, [ERROR InvalidOperationIO])
        else loop break block (newObjects, newVariables, newFunctions, newOutStack)

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
                                                            let newFunctions = Map.insert (getUNKNOWN a) (objects Map.! getCODEBLOCK b) functions
                                                            (newObjects, newFunctions, rest)
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
