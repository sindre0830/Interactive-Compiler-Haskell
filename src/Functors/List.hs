module Functors.List
    ( module Functors.List
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcEmpty :: StackState
funcEmpty = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "empty"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                let value   | not (isLIST a) = ERROR ExpectedList
                            | otherwise = BOOL (null (objects Map.! getLIST a))
                (deallocateObject a objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcHead :: StackState
funcHead = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "head"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                if not (isLIST a)
                    then (deallocateObject a objects, ERROR ExpectedList : rest)
                else do
                    let list = objects Map.! getLIST a
                    if null list
                        then (deallocateObject a objects, rest)
                    else do
                        let (value, newObjects) = duplicateObject (head list) objects
                        (deallocateObject a newObjects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcTail :: StackState
funcTail = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "tail"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                if not (isLIST a)
                    then (deallocateObject a objects, ERROR ExpectedList : rest)
                else do
                    let key = getLIST a
                    let list = objects Map.! key
                    let newObjects  | null list = objects
                                    | otherwise = updateObject key (tail list) objects
                    (deallocateObject (head list) newObjects, outStack))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcCons :: StackState
funcCons = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "cons"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                if not (isLIST b)
                    then (deallocateStack [a,b] objects, ERROR ExpectedList : rest)
                else do
                    let key = getLIST b
                    let newObjects = updateObject key (a : (objects Map.! key)) objects
                    (newObjects, b : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcAppend :: StackState
funcAppend = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "append"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                if not (isLIST a) || not (isLIST b)
                    then (deallocateStack [a,b] objects, ERROR ExpectedList : rest)
                else do
                    let keyA = getLIST a
                    let keyB = getLIST b
                    let newObjects = updateObject keyB ((objects Map.! keyA) ++ (objects Map.! keyB)) objects
                    (deallocateObject a newObjects, b : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcLength :: StackState
funcLength = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "length"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                let value   | isSTRING a = INT (toInteger $ length $ getSTRING a)
                            | isLIST a = INT (toInteger $ length $ objects Map.! getLIST a)
                            | isCODEBLOCK a = INT (toInteger $ length $ objects Map.! getCODEBLOCK a)
                            | otherwise = ERROR ExpectedList
                (deallocateObject a objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
