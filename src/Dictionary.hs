module Dictionary
    ( module Dictionary
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

type Data = String
type Key = String

type Tokens = [String]

type Token = String


data EitherN a b c d e f g h i
    = INT a | FLOAT b | BOOL c | STRING d | FUNC e | UNKNOWN f | LIST g | CODEBLOCK h | ERROR i
    deriving (Eq, Show)

getINT :: EitherN a b c d e f g h i -> a
getINT (INT a) = a

getFLOAT :: EitherN a b c d e f g h i -> b
getFLOAT (FLOAT b) = b

getBOOL :: EitherN a b c d e f g h i -> c
getBOOL (BOOL c) = c

getSTRING :: EitherN a b c d e f g h i -> d
getSTRING (STRING d) = d

getFUNC :: EitherN a b c d e f g h i -> e
getFUNC (FUNC e) = e

getUNKNOWN :: EitherN a b c d e f g h i -> f
getUNKNOWN (UNKNOWN f) = f

getLIST :: EitherN a b c d e f g h i -> g
getLIST (LIST g) = g

getCODEBLOCK :: EitherN a b c d e f g h i -> h
getCODEBLOCK (CODEBLOCK h) = h

getERROR :: EitherN a b c d e f g h i -> i
getERROR (ERROR i) = i

type List = Key

type CodeBlock = Key

type Func = Data

type Error = Data

type Unknown = Data

type Type = EitherN Int Float Bool String Func Unknown List CodeBlock Error

type Stack = [Type]

type Variable = Map Key Type

type Object = Map Key Stack

type StackState = State Stack (Object, Variable) 

type FuncInfo = Map Key Int

-- | List of functors and their amount of parameters.
functors :: FuncInfo
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

data ErrorTypes
    = StackEmpty
    | UnknownValue
    | ExpectedBool
    | ExpectedStringOfInteger
    | ExpectedStringOfFloat
    | ExpectedNumber
    | ExpectedEnumerable
    | ExpectedCodeblock
    | ExpectedList
    | ExpectedVariable
    | ExpectedString
    | DivisionByZero
    | ProgramFinishedWithMultipleValues
    | NumberConversionError    
    | IncompleteString
    | IncompleteList
    | IncompleteCodeBlock
    | InvalidListType
    | InvalidList
    | InvalidCodeblock
    | InvalidParamaterAmount
    | InvalidType
    | EmptyList
    deriving (Eq, Show)
