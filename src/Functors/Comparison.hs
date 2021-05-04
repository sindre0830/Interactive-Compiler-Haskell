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
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "=="
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value = BOOL $ compareStacks [a] [b] objects
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


compareStacks :: Stack -> Stack -> Objects -> Bool
compareStacks [] [] _ = True
compareStacks [] _ _ = False
compareStacks _ [] _ = False
compareStacks (x:xs) (y:ys) objects
    | isLIST x && isLIST y = compareStacks (objects Map.! getLIST x) (objects Map.! getLIST y) objects
    | isCODEBLOCK x && isCODEBLOCK y = compareStacks (objects Map.! getCODEBLOCK x) (objects Map.! getCODEBLOCK y) objects
    | isINT x && isFLOAT y = convertFloat x == getFLOAT y
    | isFLOAT x && isINT y = getFLOAT x == convertFloat y
    | x /= y = False
    | otherwise = compareStacks xs ys objects


funcLess :: StackState
funcLess = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "<"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = BOOL  (getINT a       < getINT b)
                            | isINT a && isFLOAT b      = BOOL  (convertFloat a < getFLOAT b)
                            | isFLOAT a && isINT b      = BOOL  (getFLOAT a     < convertFloat b)
                            | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     < getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcGreater :: StackState
funcGreater = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! ">"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = BOOL  (getINT a       > getINT b)
                            | isINT a && isFLOAT b      = BOOL  (convertFloat a > getFLOAT b)
                            | isFLOAT a && isINT b      = BOOL  (getFLOAT a     > convertFloat b)
                            | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     > getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
