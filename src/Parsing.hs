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

tokenize :: String -> Tokens
tokenize = words

parser :: Tokens -> Stack -> Object -> (Stack, Object)
parser [] stack objects = (stack, objects)
parser (x:xs) stack objects = do
    case x of
        ['"'] -> do
            let (value, rest) = stringParser xs ""
            parser rest (value : stack) objects
        _ -> parser xs (typeParser x : stack) objects

listParser :: Tokens -> Stack -> Object -> (Stack, Tokens, Object)
listParser [] _ objects = ([ERROR (show IncompleteList)], [], objects)
listParser (x:xs) stack objects = do
    case x of
        "]" -> (stack, xs, objects)
        "[" -> do
            let key = show (Map.size objects)
            let updatedObjects = Map.insert key [] objects
            let (newStack, rest, newObjects) = listParser xs [] updatedObjects
            let objects = Map.insert key newStack newObjects
            listParser rest (LIST key : stack) objects 
        _ -> do
            listParser xs ((typeParser x) : stack) objects

stringParser :: Tokens -> Data -> (Type, Tokens)
stringParser [] _ = (ERROR (show IncompleteString), [])
stringParser (x:xs) str = do
    case x of
        ['"'] -> (STRING str, xs)
        _ -> do
            if null str 
                then stringParser xs x
            else stringParser xs (str ++ " " ++ x)

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
