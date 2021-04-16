module Parsing 
    ( module Parsing
    ) where
-- foreign modules
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isJust )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
-- local modules
import Dictionary

parser :: Tokens -> Stack -> Object -> (Stack, Object)
parser [] stack objects = (stack, objects)
parser (x:xs) stack objects = do
    case x of
        _ -> parser xs (typeParser x : stack) objects


tokenize :: String -> Tokens
tokenize = words

typeParser :: Token -> Type
typeParser value = do
    if isJust (readMaybe value :: Maybe Int)
        then INT (fromJust (readMaybe value :: Maybe Int))
    else if isJust (readMaybe value :: Maybe Float)
        then FLOAT (fromJust (readMaybe value :: Maybe Float))
    else if isJust (readMaybe value :: Maybe Bool)
        then BOOL (fromJust (readMaybe value :: Maybe Bool))
    else if Map.member value functors
        then FUNC value
    else UNKNOWN value
