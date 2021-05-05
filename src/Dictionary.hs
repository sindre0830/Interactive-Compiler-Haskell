module Dictionary
    ( module Dictionary
    ) where
-- foreign modules
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (State)

-- general types
type Data = String
type Key = String
type Token = String
type Tokens = [Token]

-- types for the EitherN
type Func = Data
type Unknown = Data
type List = Key
type CodeBlock = Key
type Error = ErrorTypes
type Type = EitherN Integer Float Bool String Func Unknown List CodeBlock Error

-- stack type
type Stack = [Type]

-- types used in the stack, based on Either
data EitherN a b c d e f g h i
    = INT a | FLOAT b | BOOL c | STRING d | FUNC e | UNKNOWN f | LIST g | CODEBLOCK h | ERROR i
    deriving (Eq, Show)

{-- Gets value from a EitherN type -}

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

{-- Checks if a value is a EitherN type -}

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

-- types for the stack state
type InputStack = Stack
type Containers = Map Key Stack
type Variables = Map Key Type
type Functions = Map Key Stack
type OutputStack = Stack
type StatusIO = TypeIO

-- state type
type StackState = State (InputStack, Containers, Variables, Functions, OutputStack, StatusIO) (InputStack, Containers, Variables, Functions, OutputStack, StatusIO)


-- | Validates parameter amount for given functor.
validateParameters :: Stack -> String -> Bool
validateParameters stack functor = length stack < functors Map.! functor

-- | List of functors and their expected amount of parameters.
functors :: Map Key Int
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
        ("if", 3), ("map", 2), ("each", 2), ("foldl", 3), ("loop", 2), ("times", 2),
        -- Assignment
        (":=", 2), ("fun", 2),
        -- IO
        ("read", 0), ("print", 1)
    ]

-- | Type for each IO status.
data TypeIO
    = Input
    | Output
    | None
    deriving (Eq, Show)

-- | Type for each error.
data ErrorTypes
    = StackEmpty
    | InvalidOperationIO
    | UnknownValue
    | ExpectedBool
    | ExpectedStringOfInteger
    | ExpectedStringOfFloat
    | ExpectedNumber
    | ExpectedEnumerable
    | ExpectedCodeblock
    | ExpectedUnknown
    | ExpectedFunctor
    | ExpectedList
    | ExpectedVariable
    | ExpectedString
    | ExpectedPositiveInteger
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
