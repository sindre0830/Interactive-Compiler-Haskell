module Parser
    ( module Parser
    ) where
-- foreign modules
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isJust)
import Data.Map (Map)
import qualified Data.Map as Map
-- local modules
import Dictionary
import MemoryHandler (generateAddress, allocateMemory)

-- ! Parses tokens to a stack.
parser :: Tokens -> Stack -> Containers -> (Stack, Containers)
parser [] stack containers = (reverse stack, containers)
parser (x : xs) stack containers =
    case x of
        ['"'] -> do
            let (value, rest) = stringParser xs ""
            parser rest (value : stack) containers
        ['['] -> do
            -- parse list
            let (newStack, rest, newContainers) = listParser xs [] containers
            -- check if list is valid
            if not (null newStack) && isERROR (head newStack)
                then parser rest (head newStack : stack) newContainers
            else do
                -- update map with inner list
                let (containers, key) = allocateMemory (reverse newStack) newContainers
                parser rest (LIST key : stack) containers
        ['{'] -> do
            -- parse codeBlock
            let (newStack, rest, newContainers) = codeBlockParser xs [] containers
            -- check if codeBlock is valid
            if not (null newStack) && isERROR (head newStack)
                then parser rest (head newStack : stack) newContainers
            else do
                -- update map with inner codeBlock
                let (containers, key) = allocateMemory (reverse newStack) newContainers
                parser rest (CODEBLOCK key : stack) containers
        _ -> parser xs (typeParser x : stack) containers

-- | Parses code blocks.
codeBlockParser :: Tokens -> Stack -> Containers -> (Stack, Tokens, Containers)
codeBlockParser [] _ containers = ([ERROR IncompleteCodeBlock], [], containers)
codeBlockParser (x : xs) stack containers =
    case x of
        "}" -> (stack, xs, containers)
        "{" -> do
            -- get inner codeBlock
            let (newStack, rest, newContainers) = codeBlockParser xs [] containers
            -- update map with inner list
            let (containers, key) = allocateMemory (reverse newStack) newContainers
            codeBlockParser rest (CODEBLOCK key : stack) containers
        "[" -> do
            -- get inner list
            let (newStack, rest, newContainers) = listParser xs [] containers
            -- update map with inner codeBlock
            let (containers, key) = allocateMemory (reverse newStack) newContainers
            codeBlockParser rest (LIST key : stack) containers
        ['"'] -> do
            let (value, rest) = stringParser xs []
            codeBlockParser rest (value : stack) containers
        _ -> codeBlockParser xs (typeParser x : stack) containers

-- | Parses lists.
listParser :: Tokens -> Stack -> Containers -> (Stack, Tokens, Containers)
listParser [] _ containers = ([ERROR IncompleteList], [], containers)
listParser (x : xs) stack containers =
    case x of
        "]" -> (stack, xs, containers)
        "[" -> do
            -- get inner list
            let (newStack, rest, newContainers) = listParser xs [] containers
            -- update map with inner list
            let (containers, key) = allocateMemory (reverse newStack) newContainers
            listParser rest (LIST key : stack) containers
        "{" -> do
            -- get inner codeBlock
            let (newStack, rest, newContainers) = codeBlockParser xs [] containers
            -- update map with inner codeBlock
            let (containers, key) = allocateMemory (reverse newStack) newContainers
            listParser rest (CODEBLOCK key : stack) containers
        ['"'] -> do
            let (value, rest) = stringParser xs []
            listParser rest (value : stack) containers
        _ -> listParser xs (typeParser x : stack) containers

-- | Parses strings.
stringParser :: Tokens -> Data -> (Type, Tokens)
stringParser [] _ = (ERROR IncompleteString, [])
stringParser (x : xs) str =
    case x of
        ['"'] -> (STRING str, xs)
        _ -> if null str
                then stringParser xs x
            else stringParser xs (str ++ " " ++ x)

-- | Parses types.
typeParser :: Token -> Type
typeParser value
  | isJust (readMaybe value :: Maybe Integer)   = INT (fromJust (readMaybe value :: Maybe Integer))
  | isJust (readMaybe value :: Maybe Float)     = FLOAT (fromJust (readMaybe value :: Maybe Float))
  | isJust (readMaybe value :: Maybe Bool)      = BOOL (fromJust (readMaybe value :: Maybe Bool))
  | Map.member value functors                   = FUNC value
  | otherwise                                   = UNKNOWN value
