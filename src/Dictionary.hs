module Dictionary
    ( module Dictionary
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

type Data = String
type Name = String

type Tokens = [String]

type Variable = Map Name StackTypes


data EitherN a b c d e
    = INT a | FLOAT b | BOOL c | STRING d | OBJECT e
    deriving (Eq, Show)


getINT :: EitherN a b c d e -> a
getINT (INT a) = a

getFLOAT :: EitherN a b c d e -> b
getFLOAT (FLOAT b) = b

getBOOL :: EitherN a b c d e -> c
getBOOL (BOOL c) = c

getSTRING :: EitherN a b c d e -> d
getSTRING (STRING d) = d

getOBJECT :: EitherN a b c d e -> e
getOBJECT (OBJECT e) = e


data Type
    = FUNCTOR
    | ERROR
    | UNKNOWN
    | LIST
    | CODEBLOCK
    deriving (Eq, Show)

type Object = (Data, Type)

type StackTypes = EitherN Int Float Bool String Object

type Stack = [StackTypes] 

functors :: Map Name Int
functors = Map.fromList [
        ("+", 2), ("-", 2), ("*", 2), ("/", 2), ("div", 2), 
        ("&&", 2), ("||", 2), ("not", 1), 
        ("==", 1), ("<", 2), (">", 2), 
        ("pop", 1), ("dup", 1), ("swap", 2),
        ("length", 1),
        ("parseInteger", 1), ("parseFloat", 1), ("words", 1),
        ("empty", 1), ("head", 1), ("tail", 1), ("cons", 2), ("append", 2),
        ("exec", 1),
        ("if", 3),
        ("map", 2), ("each", 2), ("foldl", 3)
    ]
