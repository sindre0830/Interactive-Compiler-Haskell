module Converter 
    ( module Converter
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Char (toLower)
-- local modules
import Dictionary

-- | Converts type to operational stack.
getBlock :: Type -> Stack
getBlock x
    | isCODEBLOCK x = [x, FUNC "exec"]
    | otherwise = [x]

-- | Converts string to lowercase.
stringToLower :: String -> String
stringToLower = map toLower

-- | Splits string by whitespace.
tokenize :: String -> Tokens
tokenize = words

-- | Converts value of type EitherN INT to a float value.
convertFloat :: Integral a => EitherN a b c d e f g h i -> Float
convertFloat (INT a) = fromIntegral a

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
