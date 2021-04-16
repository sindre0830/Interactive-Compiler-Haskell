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

-- parser :: Tokens -> State Stack Variable
-- parser [] = return ()
-- parser (x:xs) = do
--     case x of
--         ['"']   -> do
--             let (value, rest) = stringParser xs ("", STRING) 
--             parser rest variables ++ [value]
--         ['{']   -> do
--             let (value, rest) = codeblockParser xs ("", CODEBLOCK) 
--             parser rest variables ++ [validate value variables]
--         ['[']   -> do
--             let (value, rest) = listParser xs ("", LIST) 
--             parser rest variables ++ [validate value variables]
--         _       -> parser xs variables ++ [typeParser x variables]

tokenize :: String -> Tokens
tokenize = words

typeParser :: String -> StackTypes
typeParser value = do
    if isJust (readMaybe value :: Maybe Int)
        then INT (fromJust (readMaybe value :: Maybe Int))
    else if isJust (readMaybe value :: Maybe Float)
        then FLOAT (fromJust (readMaybe value :: Maybe Float))
    else if isJust (readMaybe value :: Maybe Bool)
        then BOOL (fromJust (readMaybe value :: Maybe Bool))
    else if Map.member value functors
        then OBJECT (value, FUNCTOR)
    else OBJECT (value, UNKNOWN)
