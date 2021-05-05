module MemoryHandler
    ( module MemoryHandler
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
-- local modules
import Dictionary

-- | Generates an address to create a new container.
generateAddress :: Containers -> String
generateAddress containers = show $ getAvailableAddress containers 0

-- | Finds a valid address.
getAvailableAddress :: Containers -> Int -> Int
getAvailableAddress containers index = do
    if Map.member (show index) containers
        then getAvailableAddress containers (index + 1)
    else index

-- | Get address of given container.
getAddress :: Type -> Key
getAddress x
    | isLIST x = getLIST x
    | isCODEBLOCK x = getCODEBLOCK x

-- | Duplicate stack.
duplicateStack :: Stack -> (Stack, Containers) -> (Stack, Containers)
duplicateStack [] (stack, containers) = (reverse stack, containers)
duplicateStack (x : xs) (stack, containers) = do
        let (value, newContainers) = duplicateValue x containers
        duplicateStack xs (value : stack, newContainers)

-- | Duplicates a value.
duplicateValue :: Type -> Containers -> (Type, Containers)
duplicateValue x containers
    | isLIST x      = do
        let list = containers Map.! getLIST x
        let (stack, newContainers) = duplicateStack list ([], containers)
        let (containers, key) = allocateMemory stack newContainers
        (LIST key, containers)
    | isCODEBLOCK x = do
        let block = containers Map.! getCODEBLOCK x
        let (stack, newContainers) = duplicateStack block ([], containers)
        let (containers, key) = allocateMemory stack newContainers
        (CODEBLOCK key, containers)
    | otherwise     = (x, containers)

-- | Allocate space in memory and add a container.
allocateMemory :: Stack -> Containers -> (Containers, Key)
allocateMemory stack containers = do
    let key = generateAddress containers
    (Map.insert key stack containers, key)

-- | Deallocates any containers from the stack.
deallocateStack :: Stack -> Containers -> Containers
deallocateStack xs containers = foldl (flip deallocateMemory) containers xs

-- | Deallocates a value by also check for nested containers.
deallocateMemory :: Type -> Containers -> Containers
deallocateMemory x containers
    | isLIST x      = do
        let list = containers Map.! getLIST x
        let newContainers = deallocateStack list containers
        Map.delete (getLIST x) newContainers
    | isCODEBLOCK x = do
        let block = containers Map.! getCODEBLOCK x
        let newContainers = deallocateStack block containers
        Map.delete (getCODEBLOCK x) newContainers
    | otherwise     = containers

-- | Deallocates only the root container. Useful when you need the nested containers in memory.
deallocateRootContainer :: Type -> Containers -> Containers
deallocateRootContainer x containers
    | isLIST x      = Map.delete (getLIST x) containers
    | isCODEBLOCK x = Map.delete (getCODEBLOCK x) containers
    | otherwise     = containers

-- | Update value in the container.
updateContainer :: Type -> Stack -> Containers -> Containers
updateContainer x = Map.insert (getAddress x)

-- | Gets values from the container.
getContainer :: Containers -> Type -> Stack
getContainer containers container
    | isLIST container      = containers Map.! getLIST container
    | isCODEBLOCK container = containers Map.! getCODEBLOCK container

-- | Checks if the value is assigned to a function.
isFunction :: Type -> Functions -> Bool
isFunction x functions = isUNKNOWN x && Map.member (getUNKNOWN x) functions

-- | Checks if the value is assigned to a variable.
isVariable :: Type -> Variables -> Bool
isVariable x variables = isUNKNOWN x && Map.member (getUNKNOWN x) variables
