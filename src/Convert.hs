module Convert 
    ( module Convert
    ) where
-- foreign modules
import Data.Char (toLower)
-- local modules
import Dictionary

-- | Converts string to lowercase.
stringToLower :: String -> String
stringToLower = map toLower

-- | Splits string by whitespace.
tokenize :: String -> Tokens
tokenize = words

-- | Converts value of type EitherN INT to a float value.
convertFloat :: Integral a => EitherN a b c d e f g h i -> Float
convertFloat (INT a) = fromIntegral a
