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

deallocateStack :: Stack -> Objects -> Objects
deallocateStack xs objects = foldl (flip deallocateObject) objects xs

deallocateStack' :: Stack -> Objects -> (Stack, Objects)
deallocateStack' [] objects = ([ERROR InvalidParameterAmount], objects)
deallocateStack' (x:xs) objects = deallocateStack' xs (deallocateObject x objects)

generateObjectAddress :: Objects -> String
generateObjectAddress objects = show $ getValidAddress objects 0

getValidAddress :: Objects -> Int -> Int
getValidAddress objects index = do
    if Map.member (show index) objects
        then getValidAddress objects (index + 1)
    else index

updateObject :: Key -> Stack -> Objects -> Objects
updateObject = Map.insert

allocateObject :: Stack -> Objects -> (Objects, Key)
allocateObject stack objects = do
    let key = generateObjectAddress objects
    (Map.insert key stack objects, key)

duplicateStack :: Stack -> (Stack, Objects) -> (Stack, Objects)
duplicateStack [] (stack, objects) = (reverse stack, objects)
duplicateStack (x:xs) (stack, objects) = do
        let (value, newObjects) = duplicateObject x objects
        duplicateStack xs (value : stack, newObjects)

duplicateObject :: Type -> Objects -> (Type, Objects)
duplicateObject x objects
    | isLIST x = do
        let list = objects Map.! getLIST x
        let (stack, newObjects) = duplicateStack list ([], objects)
        let (objects, key) = allocateObject stack newObjects
        (LIST key, objects)
    | isCODEBLOCK x = do
        let block = objects Map.! getCODEBLOCK x
        let (stack, newObjects) = duplicateStack block ([], objects)
        let (objects, key) = allocateObject stack newObjects
        (CODEBLOCK key, objects)
    | otherwise = (x, objects)

deallocateOneObject :: Type -> Objects -> Objects
deallocateOneObject x objects
    | isLIST x = Map.delete (getLIST x) objects
    | isCODEBLOCK x = Map.delete (getCODEBLOCK x) objects
    | otherwise = objects

deallocateObject :: Type -> Objects -> Objects
deallocateObject x objects
    | isLIST x = do
        let list = objects Map.! getLIST x
        let newObjects = deallocateStack list objects
        Map.delete (getLIST x) newObjects
    | isCODEBLOCK x = do
        let block = objects Map.! getCODEBLOCK x
        let newObjects = deallocateStack block objects
        Map.delete (getCODEBLOCK x) newObjects
    | otherwise = objects

printableStack :: (InputStack, Objects, Variables, Functions, OutputStack, StatusIO) -> String
printableStack (_, objects, _, _, outStack, _) = "[" ++ intercalate "," (formatStack (reverse outStack) objects) ++ "]"

formatStack :: Stack -> Objects -> [String]
formatStack [] _ = []
formatStack (x:xs) objects
    | isINT x       = show (getINT x) : formatStack xs objects
    | isFLOAT x     = show (getFLOAT x) : formatStack xs objects
    | isBOOL x      = show (getBOOL x) : formatStack xs objects
    | isSTRING x    = show (getSTRING x) : formatStack xs objects
    | isFUNC x      = getFUNC x : formatStack xs objects
    | isUNKNOWN x   = getUNKNOWN x : formatStack xs objects
    | isLIST x      = printableStack ([], objects, Map.empty, Map.empty, reverse (objects Map.! getLIST x), None) : formatStack xs objects
    | isCODEBLOCK x = ("{" ++ unwords (formatStack (objects Map.! getCODEBLOCK x) objects) ++ "}") : formatStack xs objects
    | isERROR x     = show (getERROR x) : formatStack xs objects
    | otherwise     = formatStack xs objects
