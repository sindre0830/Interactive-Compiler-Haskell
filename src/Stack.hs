module Stack
    ( module Stack
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
-- local modules
import Dictionary

deallocateStack :: Stack -> Object -> (Stack, Object)
deallocateStack [] objects = ([ERROR InvalidParameterAmount], objects)
deallocateStack (x:xs) objects = deallocateStack xs (deallocateObject x objects)

generateObjectAddress :: Object -> String 
generateObjectAddress objects = show $ getValidAddress objects 0

getValidAddress :: Object -> Int -> Int
getValidAddress objects index = do
    if Map.member (show index) objects
        then getValidAddress objects (index + 1)
    else index

updateObject :: Key -> Stack -> Object -> Object
updateObject = Map.insert

allocateObject :: Stack -> Object -> Object
allocateObject stack objects = do
    let key = generateObjectAddress objects
    Map.insert key stack objects

deallocateObject :: Type -> Object -> Object
deallocateObject x objects
    | isLIST x = Map.delete (getLIST x) objects
    | isCODEBLOCK x = Map.delete (getCODEBLOCK x) objects
    | otherwise = objects

printableStack :: (Object, Stack) -> String 
printableStack (objects, stack) = "[" ++ (formatStack stack ", " objects) ++ "]"

formatStack :: Stack -> Divider -> Object -> String
formatStack [] _ _ = []
formatStack (x:xs) divider objects
    | null xs && not (null divider) = formatStack [x] "" objects
    | isINT x       = show (getINT x) ++ divider ++ formatStack xs divider objects
    | isFLOAT x     = show (getFLOAT x) ++ divider ++ formatStack xs divider objects
    | isBOOL x      = show (getBOOL x) ++ divider ++ formatStack xs divider objects
    | isSTRING x    = show (getSTRING x) ++ divider ++ formatStack xs divider objects
    | isFUNC x      = getFUNC x ++ divider ++ formatStack xs divider objects
    | isUNKNOWN x   = show (getUNKNOWN x) ++ divider ++ formatStack xs divider objects
    | isLIST x      = printableStack (objects, objects Map.! getLIST x) ++ divider ++ formatStack xs divider objects
    | isCODEBLOCK x = "{" ++ (formatStack (objects Map.! getCODEBLOCK x) ", " objects) ++ "}" ++ divider ++ formatStack xs divider objects
    | isERROR x     = show (getERROR x) ++ divider ++ formatStack xs divider objects
    | otherwise     = formatStack xs divider objects
