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
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "empty"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (a:rest) = outStack
                let newObjects = deallocateObject a objects
                let value   | not (isLIST a) = ERROR ExpectedList
                            | otherwise = BOOL (null (objects Map.! getLIST a))
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcHead :: StackState
funcHead = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "head"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (a:rest) = outStack
                let newObjects = deallocateObject a objects
                if not (isLIST a)
                    then (ERROR ExpectedList : rest, newObjects)
                else do
                    let list = objects Map.! getLIST a
                    if null list
                        then (rest, newObjects)
                    else (head list : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcTail :: StackState
funcTail = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "tail"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
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
                    (outStack, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcCons :: StackState
funcCons = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "cons"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (b:a:rest) = outStack
                if not (isLIST b)
                    then do
                        let newObjects = deallocateObject a (deallocateObject b newObjects)
                        (ERROR ExpectedList : rest, newObjects)
                else do
                    let key = getLIST b
                    let newObjects = updateObject key (a : (objects Map.! key)) objects
                    (b : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcAppend :: StackState
funcAppend = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "append"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
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
                    (b : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcLength :: StackState
funcLength = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "length"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (a:rest) = outStack
                let newObjects = deallocateObject a objects
                let value   | isSTRING a = INT (toInteger $ length $ getSTRING a)
                            | isLIST a = INT (toInteger $ length $ objects Map.! getLIST a)
                            | isCODEBLOCK a = INT (toInteger $ length $ objects Map.! getCODEBLOCK a)
                            | otherwise = ERROR ExpectedList
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
