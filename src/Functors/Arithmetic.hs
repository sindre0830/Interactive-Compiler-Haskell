module Functors.Arithmetic
    ( module Functors.Arithmetic
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcAddition :: StackState
funcAddition = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "+"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = INT      (getINT a       + getINT b)
                            | isINT a && isFLOAT b      = FLOAT    (convertFloat a + getFLOAT b)
                            | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     + convertFloat b)
                            | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     + getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcSubtraction :: StackState
funcSubtraction = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "-"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = INT      (getINT a       - getINT b)
                            | isINT a && isFLOAT b      = FLOAT    (convertFloat a - getFLOAT b)
                            | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     - convertFloat b)
                            | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     - getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcMultiplication :: StackState
funcMultiplication = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "*"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = INT      (getINT a       * getINT b)
                            | isINT a && isFLOAT b      = FLOAT    (convertFloat a * getFLOAT b)
                            | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     * convertFloat b)
                            | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     * getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcDivisionFloat :: StackState
funcDivisionFloat = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "/"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = FLOAT    (convertFloat a / convertFloat b)
                            | isINT a && isFLOAT b      = FLOAT    (convertFloat a / getFLOAT b)
                            | isFLOAT a && isINT b      = FLOAT    (getFLOAT a     / convertFloat b)
                            | isFLOAT a && isFLOAT b    = FLOAT    (getFLOAT a     / getFLOAT b)
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcDivisionInteger :: StackState
funcDivisionInteger = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if length outStack < functors Map.! "div"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (b:a:rest) = outStack
                let value   | isINT a && isINT b        = INT           (getINT a   `div` getINT b)
                            | isINT a && isFLOAT b      = INT   (floor  (convertFloat a / getFLOAT b))
                            | isFLOAT a && isINT b      = INT   (floor  (getFLOAT a     / convertFloat b))
                            | isFLOAT a && isFLOAT b    = INT   (floor  (getFLOAT a     / getFLOAT b))
                            | otherwise = ERROR ExpectedNumber
                (deallocateStack [a,b] objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
