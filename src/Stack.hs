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

deallocateObject :: Type -> Objects -> Objects
deallocateObject x objects
    | isLIST x = Map.delete (getLIST x) objects
    | isCODEBLOCK x = Map.delete (getCODEBLOCK x) objects
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
