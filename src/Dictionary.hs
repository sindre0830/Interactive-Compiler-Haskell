module Dictionary
    ( module Dictionary
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map

type Data = String
type Name = String

type Variable = Map Name StackTypes


data EitherN a b c d
    = INT a | FLOAT b | STRING c | OBJECT d
    deriving (Eq,Show)


getINT :: EitherN a b c d -> a
getINT (INT a) = a

getFLOAT :: EitherN a b c d -> b
getFLOAT (FLOAT b) = b

getSTRING :: EitherN a b c d -> c
getSTRING (STRING c) = c

getOBJECT :: EitherN a b c d -> d
getOBJECT (OBJECT d) = d


data Type
    = FUNCTOR
    | ERROR
    | UNKNOWN
    | LIST
    | CODEBLOCK

type Object = (Data, Type)

type StackTypes = EitherN Int Float String Object

type Stack = [StackTypes] 

