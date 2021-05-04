module Functors.String
    ( module Functors.String
    ) where
-- foreign modules
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (MonadState(put, get))
-- local modules
import Dictionary
import Stack


funcParseInteger :: StackState
funcParseInteger = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "parseInteger"
                then deallocateStack outStack objects
            else do
                let (a:rest) = outStack
                let newObjects = deallocateObject a objects
                let value   | not (isSTRING a) = ERROR ExpectedString
                            | isJust (readMaybe (getSTRING a) :: Maybe Integer) = INT (fromJust (readMaybe (getSTRING a) :: Maybe Integer))
                            | otherwise = ERROR ExpectedStringOfInteger
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcParseFloat :: StackState
funcParseFloat = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "parseFloat"
                then deallocateStack outStack objects
            else do
                let (a:rest) = outStack
                let newObjects = deallocateObject a objects
                let value   | not (isSTRING a) = ERROR ExpectedString
                            | isJust (readMaybe (getSTRING a) :: Maybe Float) = FLOAT (fromJust (readMaybe (getSTRING a) :: Maybe Float))
                            | otherwise = ERROR ExpectedStringOfFloat
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result


funcWords :: StackState
funcWords = do
    (inpStack, objects, variables, functions, outStack, statusIO) <- get
    let (newOutStack, newObjects) = ( do
            if length outStack < functors Map.! "words"
                then deallocateStack outStack objects
            else do
                let (a:rest) = outStack
                let (value, newObjects) | not (isSTRING a) = (ERROR ExpectedString, deallocateObject a objects)
                                        | otherwise = do
                                            let list = map STRING (words $ getSTRING a)
                                            let (newObjects, key) = allocateObject list objects
                                            (LIST key, newObjects)
                (value : rest, newObjects))
    let result = (inpStack, newObjects, variables, functions, newOutStack, statusIO)
    put result >> return result
