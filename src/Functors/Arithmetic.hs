module Functors.Arithmetic
    ( module Functors.Arithmetic
    ) where
-- foreign modules
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcAddition :: StackState
funcAddition = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "+"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                let value   | isINT a && isINT b        = INT      (getINT a       + getINT b)
                            | isINT a && isFLOAT b      = FLOAT    (convertFloat a + getFLOAT b)
                            | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     + convertFloat b)
                            | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     + getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a, b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcSubtraction :: StackState
funcSubtraction = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "-"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                let value   | isINT a && isINT b        = INT      (getINT a       - getINT b)
                            | isINT a && isFLOAT b      = FLOAT    (convertFloat a - getFLOAT b)
                            | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     - convertFloat b)
                            | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     - getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a, b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcMultiplication :: StackState
funcMultiplication = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "*"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                let value   | isINT a && isINT b        = INT      (getINT a       * getINT b)
                            | isINT a && isFLOAT b      = FLOAT    (convertFloat a * getFLOAT b)
                            | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     * convertFloat b)
                            | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     * getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a, b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcDivisionFloat :: StackState
funcDivisionFloat = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "/"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                let value   | isZero a b                = ERROR DivisionByZero
                            | isINT a && isINT b        = FLOAT    (convertFloat a / convertFloat b)
                            | isINT a && isFLOAT b      = FLOAT    (convertFloat a / getFLOAT b)
                            | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     / convertFloat b)
                            | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     / getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a, b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result


funcDivisionInteger :: StackState
funcDivisionInteger = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "div"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (b : a : rest) = outStack
                let value   | isZero a b                = ERROR DivisionByZero
                            | isINT a && isINT b        = INT           (getINT a   `div` getINT b)
                            | isINT a && isFLOAT b      = INT   (floor  (convertFloat a / getFLOAT b))
                            | isFLOAT a && isINT b      = INT   (floor  (getFLOAT a     / convertFloat b))
                            | isFLOAT a && isFLOAT b    = INT   (floor  (getFLOAT a     / getFLOAT b))
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a, b] containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

isZero :: Type -> Type -> Bool
isZero x y = x == INT 0 || x == FLOAT 0 || y == INT 0 || y == FLOAT 0
