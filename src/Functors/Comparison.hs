module Functors.Comparison
    ( module Functors.Comparison
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcEqual :: StackState
funcEqual = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "=="
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b objects)
                let value = compareStack [a] [b] objects
                (BOOL value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


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
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "<"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b objects)
                let value   | isINT a && isINT b        = BOOL  (getINT a       < getINT b)
                            | isINT a && isFLOAT b      = BOOL  (convertFloat a < getFLOAT b)
                            | isFLOAT a && isINT b      = BOOL  (getFLOAT a     < convertFloat b)
                            | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     < getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcGreater :: StackState
funcGreater = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! ">"
                then ([ERROR InvalidParameterAmount], deallocateStack outStack objects)
            else do
                let (b:a:rest) = outStack
                let newObjects = deallocateObject a (deallocateObject b objects)
                let value   | isINT a && isINT b        = BOOL  (getINT a       > getINT b)
                            | isINT a && isFLOAT b      = BOOL  (convertFloat a > getFLOAT b)
                            | isFLOAT a && isINT b      = BOOL  (getFLOAT a     > convertFloat b)
                            | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     > getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
