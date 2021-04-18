module Compiler
    ( module Compiler
    ) where
-- foreign modules
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isJust )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
-- local modules
import Dictionary
import Parsing

funcHead :: StackState
funcHead = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) = (if length stack < functors Map.! "head"
                                    then ([ERROR InvalidParameterAmount], objects)
                                else do
                                    let (a:rest) = stack
                                    if not $ isLIST a
                                        then (ERROR ExpectedList : rest, objects)
                                    else do
                                        let key = getLIST a
                                        let newObjects = Map.delete key objects
                                        (head (objects Map.! key) : rest, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, newStack)

funcTail :: StackState
funcTail = do
    (objects, variables, stack) <- get
    let (newStack, newObjects) = (if length stack < functors Map.! "tail"
                                    then ([ERROR InvalidParameterAmount], objects)
                                else do
                                    let (a:rest) = stack
                                    if not $ isLIST a
                                        then (ERROR ExpectedList : rest, objects)
                                    else do
                                        let key = getLIST a
                                        let newObjects = Map.insert key (tail (objects Map.! key)) objects
                                        (stack, newObjects))
    put (newObjects, variables, newStack)
    return (newObjects, newStack)
