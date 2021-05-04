module Stack
    ( module Stack
    ) where
-- foreign modules
import Data.Map (Map)
import Data.List ( intercalate )
import qualified Data.Map as Map
-- local modules
import Dictionary

validateParameters :: Stack -> String -> Bool
validateParameters stack functor = length stack < functors Map.! functor

deallocateStack :: Stack -> Containers -> Containers
deallocateStack xs containers = foldl (flip deallocateMemory) containers xs

generateAddress :: Containers -> String
generateAddress containers = show $ getAvailableAddress containers 0

getAvailableAddress :: Containers -> Int -> Int
getAvailableAddress containers index = do
    if Map.member (show index) containers
        then getAvailableAddress containers (index + 1)
    else index

updateContainer :: Key -> Stack -> Containers -> Containers
updateContainer = Map.insert

allocateMemory :: Stack -> Containers -> (Containers, Key)
allocateMemory stack containers = do
    let key = generateAddress containers
    (Map.insert key stack containers, key)

duplicateStack :: Stack -> (Stack, Containers) -> (Stack, Containers)
duplicateStack [] (stack, containers) = (reverse stack, containers)
duplicateStack (x:xs) (stack, containers) = do
        let (value, newContainers) = duplicateValue x containers
        duplicateStack xs (value : stack, newContainers)

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

deallocateRootContainer :: Type -> Containers -> Containers
deallocateRootContainer x containers
    | isLIST x = Map.delete (getLIST x) containers
    | isCODEBLOCK x = Map.delete (getCODEBLOCK x) containers
    | otherwise = containers

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


getContainer :: Type -> Containers -> Stack
getContainer container containers
    | isLIST container = containers Map.! getLIST container
    | isCODEBLOCK container = containers Map.! getCODEBLOCK container


isFunction :: Type -> Functions -> Bool
isFunction x functions = isUNKNOWN x && Map.member (getUNKNOWN x) functions


isVariable :: Type -> Variables -> Bool
isVariable x variables = isUNKNOWN x && Map.member (getUNKNOWN x) variables


printableStack :: (InputStack, Containers, Variables, Functions, OutputStack, StatusIO) -> String
printableStack (_, containers, _, _, outStack, _) = "[" ++ intercalate "," (formatStack (reverse outStack) containers) ++ "]"

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
