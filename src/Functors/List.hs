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
