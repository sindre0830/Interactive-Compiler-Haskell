module Functors.String
    ( module Functors.String
    ) where
-- foreign modules
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust)
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcParseInteger :: StackState
funcParseInteger = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "parseInteger"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                let value   | not (isSTRING a) = ERROR ExpectedString
                            | isJust (readMaybe (getSTRING a) :: Maybe Integer) = INT (fromJust (readMaybe (getSTRING a) :: Maybe Integer))
                            | otherwise = ERROR ExpectedStringOfInteger
                (deallocateObject a objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcParseFloat :: StackState
funcParseFloat = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "parseFloat"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                let value   | not (isSTRING a) = ERROR ExpectedString
                            | isJust (readMaybe (getSTRING a) :: Maybe Float) = FLOAT (fromJust (readMaybe (getSTRING a) :: Maybe Float))
                            | otherwise = ERROR ExpectedStringOfFloat
                (deallocateObject a objects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcWords :: StackState
funcWords = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newObjects, newOutStack) = ( do
            if validateParameters outStack "words"
                then (deallocateStack outStack objects, [ERROR InvalidParameterAmount])
            else do
                let (a:rest) = outStack
                let (value, newObjects) | not (isSTRING a) = (ERROR ExpectedString, deallocateObject a objects)
                                        | otherwise = do
                                            let list = map STRING (words $ getSTRING a)
                                            let (newObjects, key) = allocateObject list objects
                                            (LIST key, newObjects)
                (newObjects, value : rest))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
