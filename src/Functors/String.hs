module Functors.String
    ( module Functors.String
    ) where
-- foreign modules
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust)
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import MemoryHandler (allocateMemory, deallocateMemory, deallocateStack)

-- | Parses a string to a integer value.
funcParseInteger :: StackState
funcParseInteger = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "parseInteger"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let value   | not (isSTRING a)                                  = ERROR ExpectedString
                            | isJust (readMaybe (getSTRING a) :: Maybe Integer) = INT (fromJust (readMaybe (getSTRING a) :: Maybe Integer))
                            | otherwise                                         = ERROR ExpectedStringOfInteger
                (deallocateMemory a containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Parses a string to a float value
funcParseFloat :: StackState
funcParseFloat = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "parseFloat"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let value   | not (isSTRING a)                                  = ERROR ExpectedString
                            | isJust (readMaybe (getSTRING a) :: Maybe Float)   = FLOAT (fromJust (readMaybe (getSTRING a) :: Maybe Float))
                            | otherwise                                         = ERROR ExpectedStringOfFloat
                (deallocateMemory a containers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result

-- | Performs words operation on a string and adds a list of strings on the stack.
funcWords :: StackState
funcWords = do
    (inpStack, containers, variables, functions, outStack, statusIO) <- get
    let (newContainers, newOutStack) = ( do
            if validateParameters outStack "words"
                then (deallocateStack outStack containers, [ERROR InvalidParameterAmount])
            else do
                let (a : rest) = outStack
                let (value, newContainers)  | not (isSTRING a)  = (ERROR ExpectedString, deallocateMemory a containers)
                                            | otherwise         = do
                                                let list = map STRING (words $ getSTRING a)
                                                let (newContainers, key) = allocateMemory list containers
                                                (LIST key, newContainers)
                (newContainers, value : rest))
    let result = (inpStack, newContainers, variables, functions, newOutStack, statusIO)
    put result >> return result
