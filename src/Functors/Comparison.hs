module Functors.Comparison
    ( module Functors.Comparison
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcEqual :: StackState
funcEqual = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "=="
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value = BOOL $ compareStacks [a] [b] containers
                (deallocateStack [a,b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


compareStacks :: Stack -> Stack -> Containers -> Bool
compareStacks [] [] _ = True
compareStacks [] _ _ = False
compareStacks _ [] _ = False
compareStacks (x:xs) (y:ys) containers
    | isLIST x && isLIST y = compareStacks (getContainer x containers) (getContainer y containers) containers
    | isCODEBLOCK x && isCODEBLOCK y = compareStacks (getContainer x containers) (getContainer y containers) containers
    | isINT x && isFLOAT y = convertFloat x == getFLOAT y
    | isFLOAT x && isINT y = getFLOAT x == convertFloat y
    | x /= y = False
    | otherwise = compareStacks xs ys containers


funcLess :: StackState
funcLess = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "<"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = BOOL  (getINT a       < getINT b)
                            | isINT a && isFLOAT b      = BOOL  (convertFloat a < getFLOAT b)
                            | isFLOAT a && isINT b      = BOOL  (getFLOAT a     < convertFloat b)
                            | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     < getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcGreater :: StackState
funcGreater = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack ">"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = BOOL  (getINT a       > getINT b)
                            | isINT a && isFLOAT b      = BOOL  (convertFloat a > getFLOAT b)
                            | isFLOAT a && isINT b      = BOOL  (getFLOAT a     > convertFloat b)
                            | isFLOAT a && isFLOAT b    = BOOL  (getFLOAT a     > getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result
