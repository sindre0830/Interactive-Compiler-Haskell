module Stack
    ( module Stack
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
-- local modules
import Dictionary

-- | Validates parameter amount for given functor.
validateParameters :: Stack -> String -> Bool
validateParameters stack functor = length stack < functors Map.! functor

-- | Deallocates any containers from the stack.
deallocateStack :: Stack -> Containers -> Containers
deallocateStack xs containers = foldl (flip deallocateMemory) containers xs

-- | Generates an address to create a new container.
generateAddress :: Containers -> String
generateAddress containers = show $ getAvailableAddress containers 0

-- | Get address of given container.
getAddress :: Type -> Key
getAddress x
    | isLIST x = getLIST x
    | isCODEBLOCK x = getCODEBLOCK x

-- | Finds a valid address.
getAvailableAddress :: Containers -> Int -> Int
getAvailableAddress containers index = do
    if Map.member (show index) containers
        then getAvailableAddress containers (index + 1)
    else index

-- | Update value in the container.
updateContainer :: Type -> Stack -> Containers -> Containers
updateContainer x = Map.insert (getAddress x)

-- | Allocate space in memory and add a container.
allocateMemory :: Stack -> Containers -> (Containers, Key)
allocateMemory stack containers = do
    let key = generateAddress containers
    (Map.insert key stack containers, key)

-- | Duplicate stack.
duplicateStack :: Stack -> (Stack, Containers) -> (Stack, Containers)
duplicateStack [] (stack, containers) = (reverse stack, containers)
duplicateStack (x : xs) (stack, containers) = do
        let (value, newContainers) = duplicateValue x containers
        duplicateStack xs (value : stack, newContainers)

-- | Duplicates a value.
duplicateValue :: Type -> Containers -> (Type, Containers)
duplicateValue x containers
    | isLIST x = do
        let list = containers Map.! getLIST x
        let (stack, newContainers) = duplicateStack list ([], containers)
        let (containers, key) = allocateMemory stack newContainers
        (LIST key, containers)
    | isCODEBLOCK x = do
        let block = containers Map.! getCODEBLOCK x
        let (stack, newContainers) = duplicateStack block ([], containers)
        let (containers, key) = allocateMemory stack newContainers
        (CODEBLOCK key, containers)
    | otherwise = (x, containers)

-- | Deallocates only the root container. Useful when you need the nested containers in memory.
deallocateRootContainer :: Type -> Containers -> Containers
deallocateRootContainer x containers
    | isLIST x = Map.delete (getLIST x) containers
    | isCODEBLOCK x = Map.delete (getCODEBLOCK x) containers
    | otherwise = containers

-- | Deallocates a value by also check for nested containers.
deallocateMemory :: Type -> Containers -> Containers
deallocateMemory x containers
    | isLIST x = do
        let list = containers Map.! getLIST x
        let newContainers = deallocateStack list containers
        Map.delete (getLIST x) newContainers
    | isCODEBLOCK x = do
        let block = containers Map.! getCODEBLOCK x
        let newContainers = deallocateStack block containers
        Map.delete (getCODEBLOCK x) newContainers
    | otherwise = containers

-- | Gets a executable block by operation.
getBlock :: Type -> Stack
getBlock x
    | isCODEBLOCK x = [x, FUNC "exec"]
    | otherwise = [x]

-- | Gets values from the container.
getContainer :: Containers -> Type -> Stack
getContainer containers container
    | isLIST container = containers Map.! getLIST container
    | isCODEBLOCK container = containers Map.! getCODEBLOCK container

-- | Checks if the value is assigned to a function.
isFunction :: Type -> Functions -> Bool
isFunction x functions = isUNKNOWN x && Map.member (getUNKNOWN x) functions

-- | Checks if the value is assigned to a variable.
isVariable :: Type -> Variables -> Bool
isVariable x variables = isUNKNOWN x && Map.member (getUNKNOWN x) variables

-- | Gets printable version of the stack.
printableStack :: (InputStack, Containers, Variables, Functions, OutputStack, StatusIO) -> String
printableStack (_, containers, _, _, outStack, _) = "[" ++ intercalate "," (formatStack (reverse outStack) containers) ++ "]"

-- | Converts stack to list of strings.
formatStack :: Stack -> Containers -> [String]
formatStack [] _ = []
formatStack (x:xs) containers
    | isINT x       = show (getINT x) : formatStack xs containers
    | isFLOAT x     = show (getFLOAT x) : formatStack xs containers
    | isBOOL x      = show (getBOOL x) : formatStack xs containers
    | isSTRING x    = show (getSTRING x) : formatStack xs containers
    | isFUNC x      = getFUNC x : formatStack xs containers
    | isUNKNOWN x   = getUNKNOWN x : formatStack xs containers
    | isLIST x      = printableStack ([], containers, Map.empty, Map.empty, reverse (containers Map.! getLIST x), None) : formatStack xs containers
    | isCODEBLOCK x = ("{" ++ unwords (formatStack (containers Map.! getCODEBLOCK x) containers) ++ "}") : formatStack xs containers
    | isERROR x     = show (getERROR x) : formatStack xs containers
    | otherwise     = formatStack xs containers
