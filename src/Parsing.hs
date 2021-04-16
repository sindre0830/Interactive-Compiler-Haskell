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

-- | Splits string by whitespace.
tokenize :: String -> Tokens
tokenize = words

-- ! parses tokens to stack and list of objects.
parser :: Tokens -> Stack -> Object -> (Stack, Object)
parser [] stack objects = (stack, objects)
parser (x:xs) stack objects = do
    case x of
        ['"'] -> do
            let (value, rest) = stringParser xs ""
            parser rest (value : stack) objects
        ['['] -> do
            -- allocate space in map
            let key = show (Map.size objects)
            let updatedObjects = Map.insert key [] objects
            -- parse list
            let (newStack, rest, newObjects) = listParser xs [] updatedObjects
            -- update allocated space in map with inner codeBlock
            let objects = Map.insert key newStack newObjects
            parser rest (LIST key : stack) objects
        ['{'] -> do
            -- allocate space in map
            let key = show (Map.size objects)
            let updatedObjects = Map.insert key [] objects
            -- parse list
            let (newStack, rest, newObjects) = codeBlockParser xs [] updatedObjects
            -- update allocated space in map with inner codeBlock
            let objects = Map.insert key newStack newObjects
            parser rest (CODEBLOCK key : stack) objects
        _ -> parser xs (typeParser x : stack) objects

-- | Parses codeBlocks.
codeBlockParser :: Tokens -> Stack -> Object -> (Stack, Tokens, Object)
codeBlockParser [] _ objects = ([ERROR (show IncompleteCodeBlock)], [], objects)
codeBlockParser (x:xs) stack objects = do
    case x of
        "}" -> (stack, xs, objects)
        "{" -> do
            -- allocate space in map
            let key = show (Map.size objects)
            let updatedObjects = Map.insert key [] objects
            -- get inner codeBlock
            let (newStack, rest, newObjects) = codeBlockParser xs [] updatedObjects
            -- update allocated space in map with inner codeBlock
            let objects = Map.insert key newStack newObjects
            codeBlockParser rest (CODEBLOCK key : stack) objects
        "[" -> do
            -- allocate space in map
            let key = show (Map.size objects)
            let updatedObjects = Map.insert key [] objects
            -- get inner list
            let (newStack, rest, newObjects) = listParser xs [] updatedObjects
            -- update allocated space in map with inner list
            let objects = Map.insert key newStack newObjects
            codeBlockParser rest (LIST key : stack) objects
        ['"'] -> do
            let (value, rest) = stringParser xs []
            codeBlockParser rest (value : stack) objects
        _ -> do
            codeBlockParser xs ((typeParser x) : stack) objects

-- | Parses lists.
listParser :: Tokens -> Stack -> Object -> (Stack, Tokens, Object)
listParser [] _ objects = ([ERROR (show IncompleteList)], [], objects)
listParser (x:xs) stack objects = do
    case x of
        "]" -> (stack, xs, objects)
        "[" -> do
            -- allocate space in map
            let key = show (Map.size objects)
            let updatedObjects = Map.insert key [] objects
            -- get inner list
            let (newStack, rest, newObjects) = listParser xs [] updatedObjects
            -- update allocated space in map with inner list
            let objects = Map.insert key newStack newObjects
            listParser rest (LIST key : stack) objects
        "{" -> do
            -- allocate space in map
            let key = show (Map.size objects)
            let updatedObjects = Map.insert key [] objects
            -- get inner codeBlock
            let (newStack, rest, newObjects) = codeBlockParser xs [] updatedObjects
            -- update allocated space in map with inner codeBlock
            let objects = Map.insert key newStack newObjects
            listParser rest (CODEBLOCK key : stack) objects
        ['"'] -> do
            let (value, rest) = stringParser xs []
            listParser rest (value : stack) objects
        _ -> do
            listParser xs ((typeParser x) : stack) objects

-- | Parses strings.
stringParser :: Tokens -> Data -> (Type, Tokens)
stringParser [] _ = (ERROR (show IncompleteString), [])
stringParser (x:xs) str = do
    case x of
        ['"'] -> (STRING str, xs)
        _ -> do
            if null str 
                then stringParser xs x
            else stringParser xs (str ++ " " ++ x)

-- | Parses types.
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
