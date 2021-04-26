module Stack
    ( module Stack
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
-- local modules
import Dictionary

deallocateStack :: Stack -> Objects -> (Stack, Objects)
deallocateStack [] objects = ([ERROR InvalidParameterAmount], objects)
deallocateStack (x:xs) objects = deallocateStack xs (deallocateObject x objects)

generateObjectAddress :: Objects -> String 
generateObjectAddress objects = show $ getValidAddress objects 0

getValidAddress :: Objects -> Int -> Int
getValidAddress objects index = do
    if Map.member (show index) objects
        then getValidAddress objects (index + 1)
    else index

updateObject :: Key -> Stack -> Objects -> Objects
updateObject = Map.insert

allocateObject :: Stack -> Objects -> (Objects, String)
allocateObject stack objects = do
    let key = generateObjectAddress objects
    (Map.insert key stack objects, key)

duplicateStack :: Stack -> (Stack, Objects) -> (Stack, Objects)
duplicateStack [] (stack, objects) = (stack, objects)
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
        (LIST key, objects)
    | otherwise = (x, objects)

deallocateObject :: Type -> Objects -> Objects
deallocateObject x objects
    | isLIST x = do
        let list = objects Map.! getLIST x
        let (_, newObjects) = deallocateStack list objects
        Map.delete (getLIST x) newObjects
    | isCODEBLOCK x = do
        let block = objects Map.! getCODEBLOCK x
        let (_, newObjects) = deallocateStack block objects
        Map.delete (getCODEBLOCK x) newObjects
    | otherwise = objects

printableStack :: (InputStack, Objects, Variables, Functions, OutputStack, StatusIO) -> String 
printableStack (_, objects, _, _, outStack, _) = "[" ++ formatStack (reverse outStack) ", " objects ++ "]"

formatStack :: Stack -> Divider -> Objects -> String
formatStack [] _ _ = []
formatStack (x:xs) divider objects
    | null xs && not (null divider) = formatStack [x] "" objects
    | isINT x       = show (getINT x) ++ divider ++ formatStack xs divider objects
    | isFLOAT x     = show (getFLOAT x) ++ divider ++ formatStack xs divider objects
    | isBOOL x      = show (getBOOL x) ++ divider ++ formatStack xs divider objects
    | isSTRING x    = show (getSTRING x) ++ divider ++ formatStack xs divider objects
    | isFUNC x      = getFUNC x ++ divider ++ formatStack xs divider objects
    | isUNKNOWN x   = getUNKNOWN x ++ divider ++ formatStack xs divider objects
    | isLIST x      = printableStack ([], objects, Map.empty, Map.empty, reverse (objects Map.! getLIST x), None) ++ divider ++ formatStack xs divider objects
    | isCODEBLOCK x = "{" ++ formatStack (reverse (objects Map.! getCODEBLOCK x)) ", " objects ++ "}" ++ divider ++ formatStack xs divider objects
    | isERROR x     = show (getERROR x) ++ divider ++ formatStack xs divider objects
    | isPRINT x     = "Should have printed: " ++ getPRINT x ++ divider ++ formatStack xs divider objects
    | otherwise     = formatStack xs divider objects
