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
    let (a:rest) = stack
    let list = (objects Map.! getLIST a)
    let newStack = (if length stack < functors Map.! "head"
                        then [(ERROR InvalidParameterAmount)]
                    else do
                        let (a:rest) = stack
                        if not $ isLIST a
                            then ERROR ExpectedList : rest
                        else head (objects Map.! getLIST a) : rest)
    return newStack
