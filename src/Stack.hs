module Stack
    ( module Stack
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
-- local modules
import Dictionary

generateObjectAddress :: Object -> String 
generateObjectAddress objects = show $ getValidAddress objects 0

getValidAddress :: Object -> Int -> Int
getValidAddress objects index = do
    if Map.member (show index) objects
        then getValidAddress objects (index + 1)
    else index

allocateObject :: Stack -> Object -> Object
allocateObject object objects = do
    let key = generateObjectAddress objects
    Map.insert key object objects

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
    | isINT x       = formatStack xs divider objects ++ divider ++ show (getINT x)
    | isFLOAT x     = formatStack xs divider objects ++ divider ++ show (getFLOAT x)
    | isBOOL x      = formatStack xs divider objects ++ divider ++ show (getBOOL x)
    | isSTRING x    = formatStack xs divider objects ++ divider ++ show (getSTRING x)
    | isFUNC x      = formatStack xs divider objects ++ divider ++ getFUNC x
    | isUNKNOWN x   = formatStack xs divider objects ++ divider ++ show (getUNKNOWN x)
    | isLIST x      = formatStack xs divider objects ++ divider ++ printableStack (objects, (objects Map.! getLIST x))
    | isCODEBLOCK x = formatStack xs divider objects ++ divider ++ "{" ++ formatStack (objects Map.! getCODEBLOCK x) ", " objects ++ "}"
    | isERROR x     = formatStack xs divider objects ++ divider ++ show (getERROR x)
    | otherwise     = formatStack xs divider objects
