module Dictionary
    ( module Dictionary
    ) where
-- foreign modules
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust, isJust )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

type Data = String
type Key = String
type Divider = String

type Tokens = [String]

type Token = String


data EitherN a b c d e f g h i
    = INT a | FLOAT b | BOOL c | STRING d | FUNC e | UNKNOWN f | LIST g | CODEBLOCK h | ERROR i
    deriving (Eq, Show)

convertFloat :: Integral a => EitherN a b c d e f g h i -> Float
convertFloat (INT a) = fromIntegral a

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

isINT :: EitherN a b c d e f g h i -> Bool
isINT (INT _) = True
isINT _ = False

isFLOAT :: EitherN a b c d e f g h i -> Bool
isFLOAT (FLOAT _) = True
isFLOAT _ = False

isBOOL :: EitherN a b c d e f g h i -> Bool
isBOOL (BOOL _) = True
isBOOL _ = False

isSTRING :: EitherN a b c d e f g h i -> Bool
isSTRING (STRING _) = True
isSTRING _ = False

isFUNC :: EitherN a b c d e f g h i -> Bool
isFUNC (FUNC _) = True
isFUNC _ = False

isUNKNOWN :: EitherN a b c d e f g h i -> Bool
isUNKNOWN (UNKNOWN _) = True
isUNKNOWN _ = False

isERROR :: EitherN a b c d e f g h i -> Bool
isERROR (ERROR _) = True
isERROR _ = False

isLIST :: EitherN a b c d e f g h i -> Bool
isLIST (LIST _) = True
isLIST _ = False

isCODEBLOCK :: EitherN a b c d e f g h i -> Bool
isCODEBLOCK (CODEBLOCK _) = True
isCODEBLOCK _ = False

type List = Key

type CodeBlock = Key

type Func = Data

type Error = ErrorTypes

type Unknown = Data

type Type = EitherN Int Float Bool String Func Unknown List CodeBlock Error

type Stack = [Type]

type Variable = Map Key Type

type Object = Map Key Stack

type StackState = State (Stack, Object, Variable, Stack) (Object, Stack)

type FuncInfo = Map Key Int

-- | List of functors and their amount of parameters.
functors :: FuncInfo
functors = Map.fromList [
        -- Arithmetic
        ("+", 2), ("-", 2), ("*", 2), ("/", 2), ("div", 2),
        -- Bool
        ("&&", 2), ("||", 2), ("not", 1), 
        -- Comparison
        ("==", 1), ("<", 2), (">", 2), 
        -- Stack
        ("pop", 1), ("dup", 1), ("swap", 2),
        -- String
        ("parseInteger", 1), ("parseFloat", 1), ("words", 1),
        -- List
        ("empty", 1), ("head", 1), ("tail", 1), ("cons", 2), ("append", 2),
        -- Length
        ("length", 1),
        -- Code block
        ("exec", 1),
        -- Control flow
        ("if", 3), ("map", 2), ("each", 2), ("foldl", 3)
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
    | ExpectedFunctor
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
    | InvalidParameterAmount
    | InvalidType
    | EmptyList
    deriving (Eq, Show)
