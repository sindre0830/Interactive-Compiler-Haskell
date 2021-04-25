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
import Stack

parseInput :: String -> StackState
parseInput input = do
    (inpStack, objects, variables, functions, outStack, readInp) <- get
    let tokens = tokenize input
    let (newInpStack, newObjects) = parser tokens outStack objects
    put (inpStack, newObjects, variables, functions, outStack, readInp)
    return (inpStack, newObjects, variables, functions, outStack, readInp)

-- | Splits string by whitespace.
tokenize :: String -> Tokens
tokenize = words

-- ! parses tokens to stack and list of objects.
parser :: Tokens -> Stack -> Objects -> (Stack, Objects)
parser [] stack objects = (reverse stack, objects)
parser (x:xs) stack objects = do
    case x of
        ['"'] -> do
            let (value, rest) = stringParser xs ""
            parser rest (value : stack) objects
        ['['] -> do
            -- parse list
            let (newStack, rest, newObjects) = listParser xs [] objects
            -- check if list is valid
            if not (null newStack) && isERROR (head newStack)
                then parser rest (head newStack : stack) newObjects
            else do
                -- update map with inner list
                let key = generateObjectAddress newObjects
                let objects = Map.insert key (reverse newStack) newObjects
                parser rest (LIST key : stack) objects
        ['{'] -> do
            -- parse codeBlock
            let (newStack, rest, newObjects) = codeBlockParser xs [] objects
            -- check if codeBlock is valid
            if not (null newStack) && isERROR (head newStack)
                then parser rest (head newStack : stack) newObjects
            else do
                -- update map with inner codeBlock
                let key = generateObjectAddress newObjects
                let objects = Map.insert key (reverse newStack) newObjects
                parser rest (CODEBLOCK key : stack) objects
        _ -> parser xs (typeParser x : stack) objects

-- | Parses codeBlocks.
codeBlockParser :: Tokens -> Stack -> Objects -> (Stack, Tokens, Objects)
codeBlockParser [] _ objects = ([ERROR IncompleteCodeBlock], [], objects)
codeBlockParser (x:xs) stack objects = do
    case x of
        "}" -> (stack, xs, objects)
        "{" -> do
            -- get inner codeBlock
            let (newStack, rest, newObjects) = codeBlockParser xs [] objects
            -- update map with inner list
            let key = generateObjectAddress newObjects
            let objects = Map.insert key (reverse newStack) newObjects
            codeBlockParser rest (CODEBLOCK key : stack) objects
        "[" -> do
            -- get inner list
            let (newStack, rest, newObjects) = listParser xs [] objects
            -- update map with inner codeBlock
            let key = generateObjectAddress newObjects
            let objects = Map.insert key (reverse newStack) newObjects
            codeBlockParser rest (LIST key : stack) objects
        ['"'] -> do
            let (value, rest) = stringParser xs []
            codeBlockParser rest (value : stack) objects
        _ -> do
            codeBlockParser xs (typeParser x : stack) objects

-- | Parses lists.
listParser :: Tokens -> Stack -> Objects -> (Stack, Tokens, Objects)
listParser [] _ objects = ([ERROR IncompleteList], [], objects)
listParser (x:xs) stack objects = do
    case x of
        "]" -> (stack, xs, objects)
        "[" -> do
            -- get inner list
            let (newStack, rest, newObjects) = listParser xs [] objects
            -- update map with inner list
            let key = generateObjectAddress newObjects
            let objects = Map.insert key (reverse newStack) newObjects
            listParser rest (LIST key : stack) objects
        "{" -> do
            -- get inner codeBlock
            let (newStack, rest, newObjects) = codeBlockParser xs [] objects
            -- update map with inner codeBlock
            let key = generateObjectAddress newObjects
            let objects = Map.insert key (reverse newStack) newObjects
            listParser rest (CODEBLOCK key : stack) objects
        ['"'] -> do
            let (value, rest) = stringParser xs []
            listParser rest (value : stack) objects
        _ -> do
            listParser xs (typeParser x : stack) objects

-- | Parses strings.
stringParser :: Tokens -> Data -> (Type, Tokens)
stringParser [] _ = (ERROR IncompleteString, [])
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
