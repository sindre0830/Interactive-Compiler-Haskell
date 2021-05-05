-- foreign modules
import Test.Hspec (Spec, hspec, shouldBe, it, describe)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
-- local modules
import Dictionary
import Parser
import MemoryHandler
import Converter
import Compiler
import Functors.Arithmetic
import Functors.Boolean
import Functors.Assignment
import Functors.IO
import Functors.Comparison
import Functors.Stack
import Functors.String
import Functors.List
import Functors.ControlFlow
import Functors.Other

-- | Function to test the compiler. Used in official tests (last test in this file).
testCompiler :: String -> String
testCompiler input = do
    let tokens = tokenize input
    let (newInpStack, newContainers) = parser tokens [] Map.empty
    printableState $ evalState executeStack (newInpStack, newContainers, Map.empty, Map.empty, [], None)

-- | Gets printable version of the state. Used for testing each functor.
printableState :: (InputStack, Containers, Variables, Functions, OutputStack, StatusIO) -> String
printableState (_, containers, _, _, outStack, _) = printableStack outStack containers

-- | Main testing program.
main :: IO ()
main = do
    hspec $ do
        -- module Functors.Arithmetic
        spec_funcAddition
        spec_funcSubtraction
        spec_funcMultiplication
        spec_funcDivisionFloat
        spec_funcDivisionInteger
        spec_isZero
        -- module Functors.Assignment
        spec_funcSetVariable
        spec_funcSetFunction
        -- module Functors.Boolean
        spec_funcAND
        spec_funcOR
        spec_funcNOT
        -- module Functors.Comparison
        spec_funcEqual
        spec_compareStacks
        spec_funcLess
        spec_funcGreater
        -- module Functors.ControlFlow
        spec_funcIf
        spec_funcTimes
        spec_loopN
        -- module Functors.IO
        spec_funcRead
        spec_funcPrint
        -- module Functors.List
        spec_funcEmpty
        spec_funcHead
        spec_funcTail
        spec_funcCons
        spec_funcAppend
        spec_funcLength
        -- module Functors.Other
        spec_funcExec
        spec_funcEach
        spec_eachOf
        -- module Functors.Stack
        spec_funcPop
        spec_funcDup
        spec_funcSwap
        -- module Functors.String
        spec_funcParseInteger
        spec_funcParseFloat
        spec_funcWords
        -- module Compiler
        spec_executeStack
        spec_searchForErrors
        spec_setVariable
        spec_skipOperation
        spec_funcMap
        spec_mapOf
        spec_funcFoldl
        spec_foldlOf
        spec_funcLoop
        spec_loop
        -- module Converter
        spec_getBlock
        spec_stringToLower
        spec_tokenize
        spec_convertFloat
        spec_printableStack
        spec_formatStack
        -- module Dictionary
        spec_validateParameters
        -- module MemoryHandler
        spec_generateAddress
        spec_getAvailableAddress
        spec_getAddress
        spec_duplicateStack
        spec_duplicateValue
        spec_allocateMemory
        spec_deallocateStack
        spec_deallocateMemory
        spec_deallocateRootContainer
        spec_updateContainer
        spec_getContainer
        spec_isFunction
        spec_isVariable
        -- module Parser
        spec_parser
        spec_codeBlockParser
        spec_listParser
        spec_stringParser
        spec_typeParser
        -- offical tests
        spec_testCompiler

{-- module Functors.Arithmetic -}

spec_funcAddition :: Spec
spec_funcAddition = do
    describe "funcAddition tests:" $ do
        it "printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[3]\"" $ do
            printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[3]"
        it "printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) returns \"[4.0]\"" $ do
            printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) `shouldBe` "[4.0]"
        it "printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) returns \"[3.5]\"" $ do
            printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) `shouldBe` "[3.5]"
        it "printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[3.5]\"" $ do
            printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[3.5]"
        it "printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableState (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcSubtraction :: Spec
spec_funcSubtraction = do
    describe "funcSubtraction tests:" $ do
        it "printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[-1]\"" $ do
            printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[-1]"
        it "printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) returns \"[-1.0]\"" $ do
            printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) `shouldBe` "[-1.0]"
        it "printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) returns \"[-1.5]\"" $ do
            printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) `shouldBe` "[-1.5]"
        it "printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[-0.5]\"" $ do
            printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[-0.5]"
        it "printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableState (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcMultiplication :: Spec
spec_funcMultiplication = do
    describe "funcMultiplication tests:" $ do
        it "printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[2]\"" $ do
            printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[2]"
        it "printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) returns \"[3.75]\"" $ do
            printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) `shouldBe` "[3.75]"
        it "printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) returns \"[2.5]\"" $ do
            printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) `shouldBe` "[2.5]"
        it "printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[3.0]\"" $ do
            printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[3.0]"
        it "printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableState (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcDivisionFloat :: Spec
spec_funcDivisionFloat = do
    describe "funcDivisionFloat tests:" $ do
        it "printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 10], None)) returns \"[5.0]\"" $ do
            printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 10], None)) `shouldBe` "[5.0]"
        it "printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, FLOAT 10.0], None)) returns \"[5.0]\"" $ do
            printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, FLOAT 10.0], None)) `shouldBe` "[5.0]"
        it "printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, INT 10], None)) returns \"[5.0]\"" $ do
            printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, INT 10], None)) `shouldBe` "[5.0]"
        it "printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 10.0], None)) returns \"[5.0]\"" $ do
            printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 10.0], None)) `shouldBe` "[5.0]"
        it "printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 0, FLOAT 10.0], None)) returns \"[DivisionByZero]\"" $ do
            printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 0, FLOAT 10.0], None)) `shouldBe` "[DivisionByZero]"
        it "printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableState (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcDivisionInteger :: Spec
spec_funcDivisionInteger = do
    describe "funcDivisionInteger tests:" $ do
        it "printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 10], None)) returns \"[5]\"" $ do
            printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 10], None)) `shouldBe` "[5]"
        it "printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, FLOAT 10.0], None)) returns \"[5]\"" $ do
            printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, FLOAT 10.0], None)) `shouldBe` "[5]"
        it "printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, INT 10], None)) returns \"[5]\"" $ do
            printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, INT 10], None)) `shouldBe` "[5]"
        it "printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 10.0], None)) returns \"[5]\"" $ do
            printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 10.0], None)) `shouldBe` "[5]"
        it "printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 0.0, FLOAT 10.0], None)) returns \"[DivisionByZero]\"" $ do
            printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 0.0, FLOAT 10.0], None)) `shouldBe` "[DivisionByZero]"
        it "printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableState (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_isZero :: Spec
spec_isZero = do
    describe "isZero tests:" $ do
        it "isZero (INT 0) (INT 5) returns True" $ do
            isZero (INT 0) (INT 5) `shouldBe` True
        it "isZero (INT 5) (FLOAT 0) returns True" $ do
            isZero (INT 5) (FLOAT 0) `shouldBe` True
        it "isZero (INT 5) (FLOAT 0.6) returns False" $ do
            isZero (INT 5) (FLOAT 0.6) `shouldBe` False

{-- module Functors.Assignment -}

spec_funcSetVariable :: Spec
spec_funcSetVariable = do
    describe "funcSetVariable tests:" $ do
        it "printableState (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, UNKNOWN \"a\"], None)) returns \"[]\"" $ do
            printableState (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, UNKNOWN "a"], None)) `shouldBe` "[]"
        it "printableState (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedUnknown]\"" $ do
            printableState (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedUnknown]"
        it "printableState (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcSetFunction :: Spec
spec_funcSetFunction = do
    describe "funcSetFunction tests:" $ do
        it "printableState (evalState funcSetFunction ([], Map.fromList [(\"0\", [FUNC \"+\"])], Map.empty, Map.empty, [CODEBLOCK \"0\", UNKNOWN \"a\"], None)) returns \"[]\"" $ do
            printableState (evalState funcSetFunction ([], Map.fromList [("0", [FUNC "+"])], Map.empty, Map.empty, [CODEBLOCK "0", UNKNOWN "a"], None)) `shouldBe` "[]"
        it "printableState (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedUnknown]\"" $ do
            printableState (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedUnknown]"
        it "printableState (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, UNKNOWN \"a\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableState (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, UNKNOWN "a"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableState (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

{-- module Functors.Boolean -}

spec_funcAND :: Spec
spec_funcAND = do
    describe "funcAND tests:" $ do
        it "printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) returns \"[True]\"" $ do
            printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) `shouldBe` "[True]"
        it "printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) returns \"[False]\"" $ do
            printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) `shouldBe` "[False]"
        it "printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) returns \"[False]\"" $ do
            printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) `shouldBe` "[False]"
        it "printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, INT 10], None)) returns \"[ExpectedBool]\"" $ do
            printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, INT 10], None)) `shouldBe` "[ExpectedBool]"
        it "printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcOR :: Spec
spec_funcOR = do
    describe "funcOR tests:" $ do
        it "printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) returns \"[True]\"" $ do
            printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) `shouldBe` "[True]"
        it "printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) returns \"[False]\"" $ do
            printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) `shouldBe` "[False]"
        it "printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) returns \"[True]\"" $ do
            printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) `shouldBe` "[True]"
        it "printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, INT 10], None)) returns \"[ExpectedBool]\"" $ do
            printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, INT 10], None)) `shouldBe` "[ExpectedBool]"
        it "printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcNOT :: Spec
spec_funcNOT = do
    describe "funcNOT tests:" $ do
        it "printableState (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) returns \"[False]\"" $ do
            printableState (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) `shouldBe` "[False]"
        it "printableState (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [BOOL False], None)) returns \"[True]\"" $ do
            printableState (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [BOOL False], None)) `shouldBe` "[True]"
        it "printableState (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) returns \"[ExpectedBool]\"" $ do
            printableState (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) `shouldBe` "[ExpectedBool]"
        it "printableState (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

{-- module Functors.Comparison -}

spec_funcEqual :: Spec
spec_funcEqual = do
    describe "funcEqual tests:" $ do
        it "printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) returns \"[True]\"" $ do
            printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) `shouldBe` "[True]"
        it "printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) returns \"[True]\"" $ do
            printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) `shouldBe` "[True]"
        it "printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) returns \"[False]\"" $ do
            printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) `shouldBe` "[False]"
        it "printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) returns \"[True]\"" $ do
            printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) `shouldBe` "[True]"
        it "printableState (evalState funcEqual ([], Map.fromList [(\"0\", [INT 1]), (\"1\", [INT 1])], Map.empty, Map.empty, [LIST \"0\", LIST \"1\"], None)) returns \"[True]\"" $ do
            printableState (evalState funcEqual ([], Map.fromList [("0", [INT 1]), ("1", [INT 1])], Map.empty, Map.empty, [LIST "0", LIST "1"], None)) `shouldBe` "[True]"
        it "printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_compareStacks :: Spec
spec_compareStacks = do
    describe "compareStacks tests:" $ do
        it "compareStacks [BOOL True, LIST \"0\"] [BOOL True, LIST \"1\"] (Map.fromList [(\"0\", [INT 1, INT 5]), (\"1\", [INT 1, INT 5])]) returns True" $ do
            compareStacks [BOOL True, LIST "0"] [BOOL True, LIST "1"] (Map.fromList [("0", [INT 1, INT 5]), ("1", [INT 1, INT 5])]) `shouldBe` True
        it "compareStacks [] [] Map.empty returns True" $ do
            compareStacks [] [] Map.empty `shouldBe` True
        it "compareStacks [BOOL True, LIST \"0\"] [BOOL True, LIST \"1\"] (Map.fromList [(\"0\", [INT 123]), (\"1\", [INT 1, INT 5])]) returns False" $ do
            compareStacks [BOOL True, LIST "0"] [BOOL True, LIST "1"] (Map.fromList [("0", [INT 123]), ("1", [INT 1, INT 5])]) `shouldBe` False
        it "compareStacks [BOOL True] [BOOL True, LIST \"1\"] (Map.fromList [(\"1\", [INT 1, INT 5])]) returns False" $ do
            compareStacks [BOOL True] [BOOL True, LIST "1"] (Map.fromList [("1", [INT 1, INT 5])]) `shouldBe` False

spec_funcLess :: Spec
spec_funcLess = do
    describe "funcLess tests:" $ do
        it "printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[True]\"" $ do
            printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[True]"
        it "printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 2.5], None)) returns \"[False]\"" $ do
            printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 2.5], None)) `shouldBe` "[False]"
        it "printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 10], None)) returns \"[False]\"" $ do
            printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 10], None)) `shouldBe` "[False]"
        it "printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[True]\"" $ do
            printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[True]"
        it "printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableState (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcGreater :: Spec
spec_funcGreater = do
    describe "funcGreater tests:" $ do
        it "printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[False]\"" $ do
            printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[False]"
        it "printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 2.5], None)) returns \"[False]\"" $ do
            printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 2.5], None)) `shouldBe` "[False]"
        it "printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 10], None)) returns \"[True]\"" $ do
            printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 10], None)) `shouldBe` "[True]"
        it "printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[False]\"" $ do
            printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[False]"
        it "printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableState (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

{-- module Functors.ControlFlow -}

spec_funcIf :: Spec
spec_funcIf = do
    describe "funcIf tests:" $ do
        it "printableState (evalState funcIf ([], Map.fromList [(\"0\", [FUNC \"+\"]), (\"1\", [FUNC \"*\"])], Map.empty, Map.empty, [CODEBLOCK \"1\", CODEBLOCK \"0\", BOOL True], None)) returns \"[]\"" $ do
            printableState (evalState funcIf ([], Map.fromList [("0", [FUNC "+"]), ("1", [FUNC "*"])], Map.empty, Map.empty, [CODEBLOCK "1", CODEBLOCK "0", BOOL True], None)) `shouldBe` "[]"
        it "printableState (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, BOOL True], None)) returns \"[]\"" $ do
            printableState (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, BOOL True], None)) `shouldBe` "[]"
        it "printableState (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedBool]\"" $ do
            printableState (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedBool]"
        it "printableState (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcTimes :: Spec
spec_funcTimes = do
    describe "funcTimes tests:" $ do
        it "printableState (evalState funcTimes ([], Map.fromList [(\"0\", [INT 10, INT 10, FUNC \"*\"])], Map.empty, Map.empty, [CODEBLOCK \"0\", INT 4], None)) returns \"[]\"" $ do
            printableState (evalState funcTimes ([], Map.fromList [("0", [INT 10, INT 10, FUNC "*"])], Map.empty, Map.empty, [CODEBLOCK "0", INT 4], None)) `shouldBe` "[]"
        it "printableState (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, INT 1], None)) returns \"[]\"" $ do
            printableState (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, INT 1], None)) `shouldBe` "[]"
        it "printableState (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedPositiveInteger]\"" $ do
            printableState (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedPositiveInteger]"
        it "printableState (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_loopN :: Spec
spec_loopN = do
    describe "loopN tests:" $ do
        it "loopN 0 [INT 10, FUNC \"*\"] ([], Map.empty) returns ([], Map.empty)" $ do
            loopN 0 [INT 10, FUNC "*"] ([], Map.empty) `shouldBe` ([], Map.empty)
        it "loopN 3 [INT 10, FUNC \"*\"] ([], Map.empty) returns ([INT 10, FUNC \"*\", INT 10, FUNC \"*\", INT 10, FUNC \"*\"], Map.empty)" $ do
            loopN 3 [INT 10, FUNC "*"] ([], Map.empty) `shouldBe` ([INT 10, FUNC "*", INT 10, FUNC "*", INT 10, FUNC "*"], Map.empty)
        it "loopN 2 [CODEBLOCK \"0\", FUNC \"exec\"] ([], Map.fromList [(\"0\", [INT 10, FUNC \"*\"])]) \
            \ returns ([CODEBLOCK \"2\", FUNC \"exec\", CODEBLOCK \"1\", FUNC \"exec\"], \
                    \  Map.fromList [(\"0\", [INT 10, FUNC \"*\"]), (\"1\", [INT 10, FUNC \"*\"]), (\"2\", [INT 10, FUNC \"*\"])])" $ do
            loopN 2 [CODEBLOCK "0", FUNC "exec"] ([], Map.fromList [("0", [INT 10, FUNC "*"])]) `shouldBe` ([CODEBLOCK "2", FUNC "exec", CODEBLOCK "1", FUNC "exec"], Map.fromList [("0", [INT 10, FUNC "*"]), ("1", [INT 10, FUNC "*"]), ("2", [INT 10, FUNC "*"])])

{-- module Functors.IO -}

spec_funcRead :: Spec
spec_funcRead = do
    describe "funcRead tests:" $ do
        it "printableState (evalState funcRead ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[]\"" $ do
            printableState (evalState funcRead ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[]"

spec_funcPrint :: Spec
spec_funcPrint = do
    describe "funcPrint tests:" $ do
        it "printableState (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [STRING \"abc\"], None)) returns \"[\"abc\"]\"" $ do
            printableState (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [STRING "abc"], None)) `shouldBe` "[\"abc\"]"
        it "printableState (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedString]\"" $ do
            printableState (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedString]"
        it "printableState (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

{-- module Functors.List -}

spec_funcEmpty :: Spec
spec_funcEmpty = do
    describe "funcEmpty tests:" $ do
        it "printableState (evalState funcEmpty ([], Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[False]\"" $ do
            printableState (evalState funcEmpty ([], Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[False]"
        it "printableState (evalState funcEmpty ([], Map.fromList [(\"0\", [])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[True]\"" $ do
            printableState (evalState funcEmpty ([], Map.fromList [("0", [])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[True]"
        it "printableState (evalState funcEmpty ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcEmpty ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcEmpty ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcEmpty ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcHead :: Spec
spec_funcHead = do
    describe "funcHead tests:" $ do
        it "printableState (evalState funcHead ([], Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[1]\"" $ do
            printableState (evalState funcHead ([], Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[1]"
        it "printableState (evalState funcHead ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcHead ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcHead ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcHead ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcTail :: Spec
spec_funcTail = do
    describe "funcTail tests:" $ do
        it "printableState (evalState funcTail ([], Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[[2,3]]\"" $ do
            printableState (evalState funcTail ([], Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[[2,3]]"
        it "printableState (evalState funcTail ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcTail ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcTail ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcTail ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcCons :: Spec
spec_funcCons = do
    describe "funcCons tests:" $ do
        it "printableState (evalState funcCons ([], Map.fromList [(\"0\", [INT 2, INT 3])], Map.empty, Map.empty, [LIST \"0\", INT 1], None)) returns \"[[1,2,3]]\"" $ do
            printableState (evalState funcCons ([], Map.fromList [("0", [INT 2, INT 3])], Map.empty, Map.empty, [LIST "0", INT 1], None)) `shouldBe` "[[1,2,3]]"
        it "printableState (evalState funcCons ([], Map.fromList [(\"0\", []), (\"1\", [])], Map.empty, Map.empty, [LIST \"1\", LIST \"0\"], None)) returns \"[[[]]]\"" $ do
            printableState (evalState funcCons ([], Map.fromList [("0", []), ("1", [])], Map.empty, Map.empty, [LIST "1", LIST "0"], None)) `shouldBe` "[[[]]]"
        it "printableState (evalState funcCons ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcCons ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcCons ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcCons ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcAppend :: Spec
spec_funcAppend = do
    describe "funcAppend tests:" $ do
        it "printableState (evalState funcAppend ([], Map.fromList [(\"0\", [INT 2, INT 3]), (\"1\", [INT 1])], Map.empty, Map.empty, [LIST \"0\", LIST \"1\"], None)) returns \"[[1,2,3]]\"" $ do
            printableState (evalState funcAppend ([], Map.fromList [("0", [INT 2, INT 3]), ("1", [INT 1])], Map.empty, Map.empty, [LIST "0", LIST "1"], None)) `shouldBe` "[[1,2,3]]"
        it "printableState (evalState funcAppend ([], Map.fromList [(\"0\", []), (\"1\", [])], Map.empty, Map.empty, [LIST \"1\", LIST \"0\"], None)) returns \"[[]]\"" $ do
            printableState (evalState funcAppend ([], Map.fromList [("0", []), ("1", [])], Map.empty, Map.empty, [LIST "1", LIST "0"], None)) `shouldBe` "[[]]"
        it "printableState (evalState funcAppend ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcAppend ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcAppend ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcAppend ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcLength :: Spec
spec_funcLength = do
    describe "funcLength tests:" $ do
        it "printableState (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [STRING \"a b c\"], None)) returns \"[5]\"" $ do
            printableState (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [STRING "a b c"], None)) `shouldBe` "[5]"
        it "printableState (evalState funcLength ([], Map.fromList [(\"0\", [INT 1, INT 2])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[2]\"" $ do
            printableState (evalState funcLength ([], Map.fromList [("0", [INT 1, INT 2])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[2]"
        it "printableState (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [STRING \"\"], None)) returns \"[0]\"" $ do
            printableState (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [STRING ""], None)) `shouldBe` "[0]"
        it "printableState (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

{-- module Functors.Other -}

spec_funcExec :: Spec
spec_funcExec = do
    describe "funcExec tests:" $ do
        it "printableState (evalState funcExec ([], Map.fromList [(\"0\", [INT 1, INT 2, INT 3, FUNC \"+\"])], Map.empty, Map.empty, [CODEBLOCK \"0\"], None)) returns \"[]\"" $ do
            printableState (evalState funcExec ([], Map.fromList [("0", [INT 1, INT 2, INT 3, FUNC "+"])], Map.empty, Map.empty, [CODEBLOCK "0"], None)) `shouldBe` "[]"
        it "printableState (evalState funcExec ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableState (evalState funcExec ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableState (evalState funcExec ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcExec ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcEach :: Spec
spec_funcEach = do
    describe "funcEach tests:" $ do
        it "printableState (evalState funcEach ([], Map.fromList [(\"0\", [INT 10, FUNC \"*\"]), (\"1\", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [CODEBLOCK \"0\", LIST \"1\"], None)) returns \"[]\"" $ do
            printableState (evalState funcEach ([], Map.fromList [("0", [INT 10, FUNC "*"]), ("1", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [CODEBLOCK "0", LIST "1"], None)) `shouldBe` "[]"
        it "printableState (evalState funcEach ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcEach ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcEach ([], Map.fromList [(\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, LIST \"0\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableState (evalState funcEach ([], Map.fromList [("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, LIST "0"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableState (evalState funcEach ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcEach ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_eachOf :: Spec
spec_eachOf = do
    describe "eachOf tests:" $ do
        it "eachOf [INT 1, INT 2, INT 3] [INT 10, FUNC \"*\"] ([], Map.empty) returns ([INT 3, INT 10, FUNC \"*\", INT 2, INT 10, FUNC \"*\", INT 1, INT 10, FUNC \"*\"], Map.empty)" $ do
            eachOf [INT 1, INT 2, INT 3] [INT 10, FUNC "*"] ([], Map.empty) `shouldBe` ([INT 3, INT 10, FUNC "*", INT 2, INT 10, FUNC "*", INT 1, INT 10, FUNC "*"], Map.empty)
        it "eachOf [] [INT 10, FUNC \"*\"] ([], Map.empty) returns ([], Map.empty)" $ do
            eachOf [] [INT 10, FUNC "*"] ([], Map.empty) `shouldBe` ([], Map.empty)

{-- module Functors.Stack -}

spec_funcPop :: Spec
spec_funcPop = do
    describe "funcPop tests:" $ do
        it "printableState (evalState funcPop ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) returns \"[]\"" $ do
            printableState (evalState funcPop ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) `shouldBe` "[]"
        it "printableState (evalState funcPop ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcPop ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcDup :: Spec
spec_funcDup = do
    describe "funcDup tests:" $ do
        it "printableState (evalState funcDup ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) returns \"[True,True]\"" $ do
            printableState (evalState funcDup ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) `shouldBe` "[True,True]"
        it "printableState (evalState funcDup ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcDup ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcSwap :: Spec
spec_funcSwap = do
    describe "funcSwap tests:" $ do
        it "printableState (evalState funcSwap ([], Map.empty, Map.empty, Map.empty, [BOOL True, INT 5], None)) returns \"[True,5]\"" $ do
            printableState (evalState funcSwap ([], Map.empty, Map.empty, Map.empty, [BOOL True, INT 5], None)) `shouldBe` "[True,5]"
        it "printableState (evalState funcSwap ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcSwap ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

{-- module Functors.String -}

spec_funcParseInteger :: Spec
spec_funcParseInteger = do
    describe "funcParseInteger tests:" $ do
        it "printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING \"100\"], None)) returns \"[100]\"" $ do
            printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING "100"], None)) `shouldBe` "[100]"
        it "printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING \"100.0\"], None)) returns \"[ExpectedStringOfInteger]\"" $ do
            printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING "100.0"], None)) `shouldBe` "[ExpectedStringOfInteger]"
        it "printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING \"abc\"], None)) returns \"[ExpectedStringOfInteger]\"" $ do
            printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING "abc"], None)) `shouldBe` "[ExpectedStringOfInteger]"
        it "printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [INT 5], None)) returns \"[ExpectedString]\"" $ do
            printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [INT 5], None)) `shouldBe` "[ExpectedString]"
        it "printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcParseFloat :: Spec
spec_funcParseFloat = do
    describe "funcParseFloat tests:" $ do
        it "printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING \"100.5\"], None)) returns \"[100.5]\"" $ do
            printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING "100.5"], None)) `shouldBe` "[100.5]"
        it "printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING \"100\"], None)) returns \"[100.0]\"" $ do
            printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING "100"], None)) `shouldBe` "[100.0]"
        it "printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING \"abc\"], None)) returns \"[ExpectedStringOfFloat]\"" $ do
            printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING "abc"], None)) `shouldBe` "[ExpectedStringOfFloat]"
        it "printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedString]\"" $ do
            printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedString]"
        it "printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcWords :: Spec
spec_funcWords = do
    describe "funcWords tests:" $ do
        it "printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING \"a b c\"], None)) returns \"[[\"a\",\"b\",\"c\"]]\"" $ do
            printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING "a b c"], None)) `shouldBe` "[[\"a\",\"b\",\"c\"]]"
        it "printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING \"a\"], None)) returns \"[[\"a\"]]\"" $ do
            printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING "a"], None)) `shouldBe` "[[\"a\"]]"
        it "printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING \"\"], None)) returns \"[[]]\"" $ do
            printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING ""], None)) `shouldBe` "[[]]"
        it "printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedString]\"" $ do
            printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedString]"
        it "printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

{-- module Compiler -}

spec_executeStack :: Spec
spec_executeStack = do
    describe "executeStack tests:" $ do
        it "printableState (evalState executeStack ([INT 1, INT 2, FUNC \"+\"], Map.empty, Map.empty, Map.empty, [], None)) returns \"[3]\"" $ do
            printableState (evalState executeStack ([INT 1, INT 2, FUNC "+"], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[3]"
        it "printableState (evalState executeStack ([UNKNOWN \"list\", LIST \"0\", FUNC \":=\", UNKNOWN \"block\", CODEBLOCK \"1\", FUNC \"fun\", UNKNOWN \"list\", UNKNOWN \"block\", FUNC \"map\"], Map.fromList [(\"0\", [INT 1, INT 2, INT 3]), (\"1\", [INT 10, FUNC \"*\"])], Map.empty, Map.empty, [], None)) returns \"[[10,20,30]]\"" $ do
            printableState (evalState executeStack ([UNKNOWN "list", LIST "0", FUNC ":=", UNKNOWN "block", CODEBLOCK "1", FUNC "fun", UNKNOWN "list", UNKNOWN "block", FUNC "map"], Map.fromList [("0", [INT 1, INT 2, INT 3]), ("1", [INT 10, FUNC "*"])], Map.empty, Map.empty, [], None)) `shouldBe` "[[10,20,30]]"
        it "printableState (evalState executeStack ([STRING \"abc\", FUNC \"print\", INT 1], Map.empty, Map.empty, Map.empty, [], None)) returns \"[\"abc\"]\"" $ do
            printableState (evalState executeStack ([STRING "abc", FUNC "print", INT 1], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[\"abc\"]"
        it "printableState (evalState executeStack ([INT 0, INT 43, FUNC \"div\", INT 1], Map.empty, Map.empty, Map.empty, [], None)) returns \"[DivisionByZero]\"" $ do
            printableState (evalState executeStack ([INT 0, INT 43, FUNC "div", INT 1], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[DivisionByZero]"
        it "printableState (evalState executeStack ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[]\"" $ do
            printableState (evalState executeStack ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[]"

spec_searchForErrors :: Spec
spec_searchForErrors = do
    describe "searchForErrors tests:" $ do
        it "searchForErrors [ERROR InvalidType] Map.empty returns True" $ do
            searchForErrors [ERROR InvalidType] Map.empty `shouldBe` True
        it "searchForErrors [INT 5, ERROR InvalidType] Map.empty returns True" $ do
            searchForErrors [INT 5, ERROR InvalidType] Map.empty `shouldBe` True
        it "searchForErrors [INT 5] Map.empty returns False" $ do
            searchForErrors [INT 5] Map.empty `shouldBe` False
        it "searchForErrors [] Map.empty returns False" $ do
            searchForErrors [] Map.empty `shouldBe` False

spec_setVariable :: Spec
spec_setVariable = do
    describe "setVariable tests:" $ do
        it "setVariable [INT 5, UNKNOWN \"abc\"] (Map.fromList [(\"abc\", INT 10)]) Map.empty (False, Map.empty, []) returns (False, Map.empty, [INT 10, INT 5])" $ do
            setVariable [INT 5, UNKNOWN "abc"] (Map.fromList [("abc", INT 10)]) Map.empty (False, Map.empty, []) `shouldBe` (False, Map.empty, [INT 10, INT 5])
        it "setVariable [INT 5, UNKNOWN \"abc\"] Map.empty (Map.fromList [(\"abc\", [INT 10, FUNC \"+\"])]) (False, Map.empty, []) returns (True, Map.empty, [FUNC \"+\", INT 10, INT 5])" $ do
            setVariable [INT 5, UNKNOWN "abc"] Map.empty (Map.fromList [("abc", [INT 10, FUNC "+"])]) (False, Map.empty, []) `shouldBe` (True, Map.empty, [FUNC "+", INT 10, INT 5])
        it "setVariable [INT 5, UNKNOWN \"abc\"] Map.empty Map.empty (False, Map.empty, []) returns (False, Map.empty, [UNKNOWN \"abc\", INT 5])" $ do
            setVariable [INT 5, UNKNOWN "abc"] Map.empty Map.empty (False, Map.empty, []) `shouldBe` (False, Map.empty, [UNKNOWN "abc", INT 5])
        it "setVariable [] Map.empty Map.empty (False, Map.empty, []) returns (False, Map.empty, [])" $ do
            setVariable [] Map.empty Map.empty (False, Map.empty, []) `shouldBe` (False, Map.empty, [])

spec_skipOperation :: Spec
spec_skipOperation = do
    describe "skipOperation" $ do
        it "skipOperation [FUNC \"read\", FUNC \"times\"] Map.empty Map.empty returns ([FUNC \"times\"], [FUNC \"read\"])" $ do
            skipOperation [FUNC "read", FUNC "times"] Map.empty Map.empty `shouldBe` ([FUNC "times"], [FUNC "read"])
        it "skipOperation [INT 5, INT 5, FUNC \"+\"] Map.empty Map.empty returns ([INT 5, INT 5, FUNC \"+\"], [])" $ do
            skipOperation [INT 5, INT 5, FUNC "+"] Map.empty Map.empty `shouldBe` ([INT 5, INT 5, FUNC "+"], [])
        it "skipOperation [] Map.empty Map.empty returns ([], [])" $ do
            skipOperation [] Map.empty Map.empty `shouldBe` ([], [])

spec_funcMap :: Spec
spec_funcMap = do
    describe "funcMap tests:" $ do
        it "printableState (evalState funcMap ([], Map.fromList [(\"0\", [INT 10, FUNC \"*\"]), (\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [CODEBLOCK \"0\", LIST \"1\"], None)) returns \"[[30,20,10]]\"" $ do
            printableState (evalState funcMap ([], Map.fromList [("0", [INT 10, FUNC "*"]), ("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [CODEBLOCK "0", LIST "1"], None)) `shouldBe` "[[30,20,10]]"
        it "printableState (evalState funcMap ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcMap ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcMap ([], Map.fromList [(\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, LIST \"0\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableState (evalState funcMap ([], Map.fromList [("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, LIST "0"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableState (evalState funcMap ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcMap ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_mapOf :: Spec
spec_mapOf = do
    describe "mapOf tests:" $ do
        it "mapOf [INT 3, INT 2, INT 1] [INT 10, FUNC \"*\"] (Map.empty, Map.empty, Map.empty, []) returns (Map.empty, Map.empty, Map.empty, [INT 10, INT 20, INT 30])" $ do
            mapOf [INT 3, INT 2, INT 1] [INT 10, FUNC "*"] (Map.empty, Map.empty, Map.empty, []) `shouldBe` (Map.empty, Map.empty, Map.empty, [INT 10, INT 20, INT 30])
        it "mapOf [INT 3, INT 2, INT 1] [CODEBLOCK \"0\", FUNC \"exec\"] (Map.fromList [(\"0\", [INT 10, FUNC \"*\"])], Map.empty, Map.empty, []) returns (Map.fromList [(\"0\", [INT 10, FUNC \"*\"])], Map.empty, Map.empty, [INT 10, INT 20, INT 30])" $ do
            mapOf [INT 3, INT 2, INT 1] [CODEBLOCK "0", FUNC "exec"] (Map.fromList [("0", [INT 10, FUNC "*"])], Map.empty, Map.empty, []) `shouldBe` (Map.fromList [("0", [INT 10, FUNC "*"])], Map.empty, Map.empty, [INT 10, INT 20, INT 30])

spec_funcFoldl :: Spec
spec_funcFoldl = do
    describe "funcFoldl tests:" $ do
        it "printableState (evalState funcFoldl ([], Map.fromList [(\"0\", [FUNC \"+\"]), (\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [CODEBLOCK \"0\", INT 0, LIST \"1\"], None)) returns \"[6]\"" $ do
            printableState (evalState funcFoldl ([], Map.fromList [("0", [FUNC "+"]), ("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [CODEBLOCK "0", INT 0, LIST "1"], None)) `shouldBe` "[6]"
        it "printableState (evalState funcFoldl ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedList]\"" $ do
            printableState (evalState funcFoldl ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedList]"
        it "printableState (evalState funcFoldl ([], Map.fromList [(\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, FUNC \"+\", LIST \"0\"], None)) returns \"[InvalidType]\"" $ do
            printableState (evalState funcFoldl ([], Map.fromList [("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, FUNC "+", LIST "0"], None)) `shouldBe` "[InvalidType]"
        it "printableState (evalState funcFoldl ([], Map.fromList [(\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, INT 0, LIST \"0\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableState (evalState funcFoldl ([], Map.fromList [("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, INT 0, LIST "0"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableState (evalState funcFoldl ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcFoldl ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_foldlOf :: Spec
spec_foldlOf = do
    describe "foldlOf tests:" $ do
        it "foldlOf [INT 3, INT 2, INT 1] [FUNC \"+\"] (Map.empty, Map.empty, Map.empty, INT 0) returns (Map.empty, Map.empty, Map.empty, INT 6)" $ do
            foldlOf [INT 3, INT 2, INT 1] [FUNC "+"] (Map.empty, Map.empty, Map.empty, INT 0) `shouldBe` (Map.empty, Map.empty, Map.empty, INT 6)
        it "foldlOf [] [FUNC \"+\"] (Map.empty, Map.empty, Map.empty, INT 0) returns (Map.empty, Map.empty, Map.empty, INT 0)" $ do
            foldlOf [] [FUNC "+"] (Map.empty, Map.empty, Map.empty, INT 0) `shouldBe` (Map.empty, Map.empty, Map.empty, INT 0)
        it "foldlOf [INT 3, INT 2, INT 1] [FUNC \"read\"] (Map.empty, Map.empty, Map.empty, STRING \"abc\") returns (Map.empty, Map.empty, Map.empty, ERROR InvalidOperationIO)" $ do
            foldlOf [INT 3, INT 2, INT 1] [FUNC "read"] (Map.empty, Map.empty, Map.empty, STRING "abc") `shouldBe` (Map.empty, Map.empty, Map.empty, ERROR InvalidOperationIO)

spec_funcLoop :: Spec
spec_funcLoop = do
    describe "funcLoop tests:" $ do
        it "printableState (evalState funcLoop ([], Map.fromList [(\"0\", [FUNC \"dup\", INT 4, FUNC \"dup\"]), (\"1\", [FUNC \"dup\", INT 1, FUNC \"+\"])], Map.empty, Map.empty, [CODEBLOCK \"1\", CODEBLOCK \"0\", INT 1], None)) returns \"[]\"" $ do
            printableState (evalState funcLoop ([], Map.fromList [("0", [FUNC "dup", INT 4, FUNC ">"]), ("1", [FUNC "dup", INT 1, FUNC "+"])], Map.empty, Map.empty, [CODEBLOCK "1", CODEBLOCK "0", INT 1], None)) `shouldBe` "[]"
        it "printableState (evalState funcLoop ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableState (evalState funcLoop ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableState (evalState funcLoop ([], Map.fromList [(\"0\", [FUNC \"dup\", INT 4, FUNC \"dup\"])], Map.empty, Map.empty, [FLOAT 5.0, CODEBLOCK \"0\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableState (evalState funcLoop ([], Map.fromList [("0", [FUNC "dup", INT 4, FUNC ">"])], Map.empty, Map.empty, [FLOAT 5.0, CODEBLOCK "0"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableState (evalState funcLoop ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableState (evalState funcLoop ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_loop :: Spec
spec_loop = do
    describe "loop tests:" $ do
        it "loop [BOOL True] [] (Map.empty, Map.empty, Map.empty, []) returns (Map.empty, [])" $ do
            loop [BOOL True] [] (Map.empty, Map.empty, Map.empty, []) `shouldBe` (Map.empty, [])
        it "loop [FUNC \"read\"] [] (Map.empty, Map.empty, Map.empty, []) returns (Map.empty, [ERROR InvalidOperationIO])" $ do
            loop [FUNC "read"] [] (Map.empty, Map.empty, Map.empty, []) `shouldBe` (Map.empty, [ERROR InvalidOperationIO])
        it "loop [BOOL False] [FUNC \"read\"] (Map.empty, Map.empty, Map.empty, []) returns (Map.empty, [ERROR InvalidOperationIO])" $ do
            loop [BOOL False] [FUNC "read"] (Map.empty, Map.empty, Map.empty, []) `shouldBe` (Map.empty, [ERROR InvalidOperationIO])

{-- module Converter -}

spec_getBlock :: Spec
spec_getBlock = do
    describe "getBlock tests:" $ do
        it "getBlock (CODEBLOCK \"0\") returns [CODEBLOCK \"0\", FUNC \"exec\"]" $ do
            getBlock (CODEBLOCK "0") `shouldBe` [CODEBLOCK "0", FUNC "exec"]
        it "getBlock (FUNC \"+\") returns [FUNC \"+\"]" $ do
            getBlock (FUNC "+") `shouldBe` [FUNC "+"]
        it "getBlock (UNKNOWN \"block\") returns [UNKNOWN \"block\"]" $ do
            getBlock (UNKNOWN "block") `shouldBe` [UNKNOWN "block"]

spec_stringToLower :: Spec
spec_stringToLower = do
    describe "stringToLower tests:" $ do
        it "stringToLower \"aBcDe\" returns \"abcde\"" $ do
            stringToLower "aBcDe" `shouldBe` "abcde"
        it "stringToLower \"abcde\" returns \"abcde\"" $ do
            stringToLower "abcde" `shouldBe` "abcde"
        it "stringToLower \"\" returns \"\"" $ do
            stringToLower "" `shouldBe` ""

spec_tokenize :: Spec
spec_tokenize = do
    describe "tokenize tests:" $ do
        it "tokenize \"1 2 +\" returns [\"1\", \"2\", \"+\"]" $ do
            tokenize "1 2 +" `shouldBe` ["1", "2", "+"]

spec_convertFloat :: Spec
spec_convertFloat = do
    describe "convertFloat tests:" $ do
        it "convertFloat (INT 5) returns 5.0" $ do
            convertFloat (INT 5) `shouldBe` 5.0
        it "convertFloat (INT 0) returns 0.0" $ do
            convertFloat (INT 0) `shouldBe` 0.0
        it "convertFloat (INT (-5)) returns -5.0" $ do
            convertFloat (INT (-5)) `shouldBe` -5.0

spec_printableStack :: Spec
spec_printableStack = do
    describe "printableStack tests:" $ do
        it "printableStack [INT 2, STRING \"a string\", INT 1] Map.empty returns \"[1,\"a string\",2\"]" $ do
            printableStack [INT 2, STRING "a string", INT 1] Map.empty `shouldBe` "[1,\"a string\",2]"

spec_formatStack :: Spec
spec_formatStack = do
    describe "formatStack tests:" $ do
        it "formatStack [INT 2, STRING \"a string\", INT 1] Map.empty returns [\"2\",\"\"a string\"\",\"1\"]" $ do
            formatStack [INT 2, STRING "a string", INT 1] Map.empty `shouldBe` ["2","\"a string\"","1"]

{-- module Dictionary -}

spec_validateParameters :: Spec
spec_validateParameters = do
    describe "validateParameters tests:" $ do
        it "validateParameters [INT 1, INT 2] \"+\" returns False" $ do
            validateParameters [INT 1, INT 2] "+" `shouldBe` False
        it "validateParameters [INT 1, INT 2, INT 3] \"+\" returns False" $ do
            validateParameters [INT 1, INT 2, INT 3] "+" `shouldBe` False
        it "validateParameters [INT 1] \"+\" returns True" $ do
            validateParameters [INT 1] "+" `shouldBe` True
        it "validateParameters [] \"+\" returns True" $ do
            validateParameters [] "+" `shouldBe` True

{-- module MemoryHandler -}

spec_generateAddress :: Spec
spec_generateAddress = do
    describe "generateAddress tests:" $ do
        it "generateAddress (Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", []), (\"1\", [])]) returns \"4\"" $ do
            generateAddress (Map.fromList [("0", []), ("3", []), ("2", []), ("5", []), ("1", [])]) `shouldBe` "4"
        it "generateAddress Map.empty returns \"0\"" $ do
            generateAddress Map.empty `shouldBe` "0"

spec_getAvailableAddress :: Spec
spec_getAvailableAddress = do
    describe "getAvailableAddress tests:" $ do
        it "getAvailableAddress (Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", []), (\"1\", [])]) 0 returns 4" $ do
            getAvailableAddress (Map.fromList [("0", []), ("3", []), ("2", []), ("5", []), ("1", [])]) 0 `shouldBe` 4
        it "getAvailableAddress Map.empty 0 returns 0" $ do
            getAvailableAddress Map.empty 0 `shouldBe` 0

spec_getAddress :: Spec
spec_getAddress = do
    describe "getAddress tests:" $ do
        it "getAddress (LIST \"66\") returns \"66\"" $ do
            getAddress (LIST "66") `shouldBe` "66"
        it "getAddress (CODEBLOCK \"666\") returns \"666\"" $ do
            getAddress (CODEBLOCK "666") `shouldBe` "666"
        it "getAddress (CODEBLOCK \"\") returns \"\"" $ do
            getAddress (CODEBLOCK "") `shouldBe` ""

spec_duplicateStack :: Spec
spec_duplicateStack = do
    describe "duplicateStack tests:" $ do
        it "duplicateStack [LIST \"0\"] ([], Map.fromList [(\"0\", [INT 45])]) returns ([LIST \"1\"], Map.fromList [(\"0\", [INT 45]), (\"1\", [INT 45])])" $ do
            duplicateStack [LIST "0"] ([], Map.fromList [("0", [INT 45])]) `shouldBe` ([LIST "1"], Map.fromList [("0", [INT 45]), ("1", [INT 45])])
        it "duplicateStack [INT 45, INT 30] ([], Map.empty) returns ([INT 45, INT 30], Map.empty)" $ do
            duplicateStack [INT 45, INT 30] ([], Map.empty) `shouldBe` ([INT 45, INT 30], Map.empty)
        it "duplicateStack [] ([], Map.empty) returns ([], Map.empty)" $ do
            duplicateStack [] ([], Map.empty) `shouldBe` ([], Map.empty)

spec_duplicateValue :: Spec
spec_duplicateValue = do
    describe "duplicateValue tests:" $ do
        it "duplicateValue (LIST \"0\") (Map.fromList [(\"0\", [INT 45])]) returns (LIST \"1\", Map.fromList [(\"0\", [INT 45]), (\"1\", [INT 45])])" $ do
            duplicateValue (LIST "0") (Map.fromList [("0", [INT 45])]) `shouldBe` (LIST "1", Map.fromList [("0", [INT 45]), ("1", [INT 45])])
        it "duplicateValue (INT 45) Map.empty returns (INT 45, Map.empty)" $ do
            duplicateValue (INT 45) Map.empty `shouldBe` (INT 45, Map.empty)

spec_allocateMemory :: Spec
spec_allocateMemory = do
    describe "allocateMemory tests:" $ do
        it "allocateMemory [INT 1] (Map.fromList [(\"0\", []), (\"2\", [])]) returns (Map.fromList [(\"0\", []), (\"2\", []), (\"1\", [INT 1])], \"1\")" $ do
            allocateMemory [INT 1] (Map.fromList [("0", []), ("2", [])]) `shouldBe` (Map.fromList [("0", []), ("2", []), ("1", [INT 1])], "1")

spec_deallocateStack :: Spec
spec_deallocateStack = do
    describe "deallocateStack tests:" $ do
        it "deallocateStack [INT 0] (Map.fromList [(\"0\", [INT 1])]) returns Map.fromList [(\"0\", [INT 1])]" $ do
            deallocateStack [INT 0] (Map.fromList [("0", [INT 1])]) `shouldBe` Map.fromList [("0", [INT 1])]
        it "deallocateStack [LIST \"0\"] (Map.fromList [(\"0\", [INT 1])]) returns Map.empty" $ do
            deallocateStack [LIST "0"] (Map.fromList [("0", [INT 1])]) `shouldBe` Map.empty
        it "deallocateStack [LIST \"1\"] (Map.fromList [(\"0\", [INT 1]), (\"1\", [LIST \"0\"])]) returns Map.empty" $ do
            deallocateStack [LIST "1"] (Map.fromList [("0", [INT 1]), ("1", [LIST "0"])]) `shouldBe` Map.empty
        it "deallocateStack [] Map.empty returns Map.empty" $ do
            deallocateStack [] Map.empty `shouldBe` Map.empty

spec_deallocateMemory :: Spec
spec_deallocateMemory = do
    describe "deallocateMemory tests:" $ do
        it "deallocateMemory (LIST \"3\") (Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", [])]) returns Map.fromList [(\"0\", []), (\"2\", []), (\"5\", [])]" $ do
            deallocateMemory (LIST "3") (Map.fromList [("0", []), ("3", []), ("2", []), ("5", [])]) `shouldBe` Map.fromList [("0", []), ("2", []), ("5", [])]
        it "deallocateMemory (INT 1) Map.empty returns Map.empty" $ do
            deallocateMemory (INT 1) Map.empty `shouldBe` Map.empty
        it "deallocateMemory (INT 1) (Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", [])]) returns Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", [])]" $ do
            deallocateMemory (INT 1) (Map.fromList [("0", []), ("3", []), ("2", []), ("5", [])]) `shouldBe` Map.fromList [("0", []), ("3", []), ("2", []), ("5", [])]

spec_deallocateRootContainer :: Spec
spec_deallocateRootContainer = do
    describe "deallocateRootContainer tests:" $ do
        it "deallocateRootContainer (CODEBLOCK \"0\") (Map.fromList [(\"0\", [LIST \"1\"]), (\"1\", [INT 42])]) returns Map.fromList [(\"1\", [INT 42])]" $ do
            deallocateRootContainer (CODEBLOCK "0") (Map.fromList [("0", [LIST "1"]), ("1", [INT 42])]) `shouldBe` Map.fromList [("1", [INT 42])]
        it "deallocateRootContainer (INT 45) (Map.fromList [(\"0\", [INT 42])]) returns Map.fromList [(\"0\", [INT 42])]" $ do
            deallocateRootContainer (INT 45) (Map.fromList [("0", [INT 42])]) `shouldBe` Map.fromList [("0", [INT 42])]

spec_updateContainer :: Spec
spec_updateContainer = do
    describe "updateContainer tests:" $ do
        it "updateContainer (LIST \"0\") [INT 1, INT 2] (Map.fromList [(\"0\", [INT 1])]) returns Map.fromList [(\"0\", [INT 1, INT 2])]" $ do
            updateContainer (LIST "0") [INT 1, INT 2] (Map.fromList [("0", [INT 1])]) `shouldBe` Map.fromList [("0", [INT 1, INT 2])]

spec_getContainer :: Spec
spec_getContainer = do
    describe "getContainer tests:" $ do
        it "getContainer (Map.fromList [(\"0\", [INT 45])]) (LIST \"0\") returns [INT 45]" $ do
            getContainer (Map.fromList [("0", [INT 45])]) (LIST "0") `shouldBe` [INT 45]
        it "getContainer (Map.fromList [(\"0\", [INT 45])]) (CODEBLOCK \"0\") returns [INT 45]" $ do
            getContainer (Map.fromList [("0", [INT 45])]) (CODEBLOCK "0") `shouldBe` [INT 45]

spec_isFunction :: Spec
spec_isFunction = do
    describe "isFunction tests:" $ do
        it "isFunction (UNKNOWN \"block\") (Map.fromList [(\"block\", [INT 45])]) returns True" $ do
            isFunction (UNKNOWN "block") (Map.fromList [("block", [INT 45])]) `shouldBe` True
        it "isFunction (UNKNOWN \"list\") (Map.fromList [(\"block\", [INT 45])]) returns False" $ do
            isFunction (UNKNOWN "list") (Map.fromList [("block", [INT 45])]) `shouldBe` False
        it "isFunction (STRING \"block\") (Map.fromList [(\"block\", [INT 45])]) returns False" $ do
            isFunction (STRING "block") (Map.fromList [("block", [INT 45])]) `shouldBe` False

spec_isVariable :: Spec
spec_isVariable = do
    describe "isVariable tests:" $ do
        it "isVariable (UNKNOWN \"block\") (Map.fromList [(\"block\", INT 45)]) returns True" $ do
            isVariable (UNKNOWN "block") (Map.fromList [("block", INT 45)]) `shouldBe` True
        it "isVariable (UNKNOWN \"list\") (Map.fromList [(\"block\", INT 45)]) returns False" $ do
            isVariable (UNKNOWN "list") (Map.fromList [("block", INT 45)]) `shouldBe` False
        it "isVariable (STRING \"block\") (Map.fromList [(\"block\", INT 45)]) returns False" $ do
            isVariable (STRING "block") (Map.fromList [("block", INT 45)]) `shouldBe` False

{-- module Parser -}

spec_parser :: Spec
spec_parser = do
    describe "parser tests:" $ do
        it "parser [\"1\", \"2\", \"+\"] [] Map.empty returns ([INT 1, INT 2, FUNC \"+\"], Map.empty)" $ do
            parser ["1", "2", "+"] [] Map.empty `shouldBe` ([INT 1, INT 2, FUNC "+"], Map.empty)
        it "parser [\"1\", \", \"a\", \"string\", \"] [] Map.empty returns ([INT 1, STRING \"a string\"], Map.empty)" $ do
            parser ["1", "\"", "a", "string", "\""] [] Map.empty `shouldBe` ([INT 1, STRING "a string"], Map.empty)
        it "parser [\", \"a\", \"string\", \", \"1\"] [] Map.empty returns ([STRING \"a string\", INT 1], Map.empty)" $ do
            parser ["\"", "a", "string", "\"", "1"] [] Map.empty `shouldBe` ([STRING "a string", INT 1], Map.empty)
        it "parser [\", \"a\", \"string\", \"] [] Map.empty returns ([STRING \"a string\"], Map.empty)" $ do
            parser ["\"", "a", "string", "\""] [] Map.empty `shouldBe` ([STRING "a string"], Map.empty)
        it "parser [\", \"a\", \"string\", \", \", \"b\", \"string\", \"] [] Map.empty returns ([STRING \"a string\", STRING \"b string\"], Map.empty)" $ do
            parser ["\"", "a", "string", "\"", "\"", "b", "string", "\""] [] Map.empty `shouldBe` ([STRING "a string", STRING "b string"], Map.empty)
        it "parser (tokenize \"{\") [] Map.empty returns ([ERROR IncompleteCodeBlock], Map.empty)" $ do
            parser (tokenize "{") [] Map.empty `shouldBe` ([ERROR IncompleteCodeBlock], Map.empty)
        it "parser (tokenize \"[\") [] Map.empty returns ([ERROR IncompleteList], Map.empty)" $ do
            parser (tokenize "[") [] Map.empty `shouldBe` ([ERROR IncompleteList], Map.empty)

spec_codeBlockParser :: Spec
spec_codeBlockParser = do
    describe "codeBlockParser tests:" $ do
        it "codeBlockParser (tokenize \"1 2 }\") [] Map.empty returns ([INT 2, INT 1], [], Map.empty)" $ do
            codeBlockParser (tokenize "1 2 }") [] Map.empty `shouldBe` ([INT 2, INT 1], [], Map.empty)
        it "codeBlockParser (tokenize \"1 2\") [] Map.empty returns ([ERROR IncompleteCodeBlock], [], Map.empty)" $ do
            codeBlockParser (tokenize "1 2") [] Map.empty `shouldBe` ([ERROR IncompleteCodeBlock], [], Map.empty)
        it "codeBlockParser (tokenize \"{ } } }\") [] Map.empty returns ([CODEBLOCK \"0\"], [], (Map.fromList [(\"0\", [])]))" $ do
            codeBlockParser (tokenize "{ } }") [] Map.empty `shouldBe` ([CODEBLOCK "0"], [], Map.fromList [("0", [])])
        it "codeBlockParser (tokenize \"1 { 2 } 3 }\") [] Map.empty returns ([INT 3, CODEBLOCK \"0\", INT 1], [], (Map.fromList [(\"0\", [INT 2])]))" $ do
            codeBlockParser (tokenize "1 { 2 } 3 }") [] Map.empty `shouldBe` ([INT 3, CODEBLOCK "0", INT 1], [], Map.fromList [("0", [INT 2])])
        it "codeBlockParser (tokenize \"{ { } } { } }\") [] Map.empty returns ([CODEBLOCK \"2\", CODEBLOCK \"1\"], [], (Map.fromList [(\"0\", []), (\"1\", [CODEBLOCK \"0\"]), (\"2\", [])]))" $ do
            codeBlockParser (tokenize "{ { } } { } }") [] Map.empty `shouldBe` ([CODEBLOCK "2", CODEBLOCK "1"], [], Map.fromList [("0", []), ("1", [CODEBLOCK "0"]), ("2", [])])
        it "codeBlockParser (tokenize \"1 \" a string \" 2 }\") [] Map.empty returns ([INT 2, STRING \"a string\", INT 1], [], Map.empty)" $ do
            codeBlockParser (tokenize "1 \" a string \" 2 }") [] Map.empty `shouldBe` ([INT 2, STRING "a string", INT 1], [], Map.empty)
        it "codeBlockParser (tokenize \"1 2 [ ] }\") [] Map.empty returns ({LIST 0, INT 2, INT 1},[], Map.empty)" $ do
            codeBlockParser (tokenize "1 2 [ ] }") [] Map.empty `shouldBe` ([LIST "0", INT 2, INT 1], [], Map.fromList [("0", [])])
        it "codeBlockParser (tokenize \"1 2 [ 4 { } ] }\") [] Map.empty returns ({LIST 1, INT 2, INT 1}, [], Map.fromList [(\"0\", []), (\"1\", [INT 4, CODEBLOCK \"0\"])])" $ do
            codeBlockParser (tokenize "1 2 [ 4 { } ] }") [] Map.empty `shouldBe` ([LIST "1", INT 2, INT 1], [], Map.fromList [("0", []), ("1", [INT 4, CODEBLOCK "0"])])

spec_listParser :: Spec
spec_listParser = do
    describe "listParser tests:" $ do
        it "listParser (tokenize \"1 2 ]\") [] Map.empty returns ([INT 2, INT 1], [], Map.empty)" $ do
            listParser (tokenize "1 2 ]") [] Map.empty `shouldBe` ([INT 2, INT 1], [], Map.empty)
        it "listParser (tokenize \"1 2\") [] Map.empty returns ([ERROR IncompleteList], [], Map.empty)" $ do
            listParser (tokenize "1 2") [] Map.empty `shouldBe` ([ERROR IncompleteList], [], Map.empty)
        it "listParser (tokenize \"[ ] ]\") [] Map.empty returns ([LIST \"0\"], [], (Map.fromList [(\"0\", [])]))" $ do
            listParser (tokenize "[ ] ]") [] Map.empty `shouldBe` ([LIST "0"], [], Map.fromList [("0", [])])
        it "listParser (tokenize \"1 [ 2 ] 3 ]\") [] Map.empty returns ([INT 3, LIST \"0\", INT 1], [], (Map.fromList [(\"0\", [INT 2])]))" $ do
            listParser (tokenize "1 [ 2 ] 3 ]") [] Map.empty `shouldBe` ([INT 3, LIST "0", INT 1], [], Map.fromList [("0", [INT 2])])
        it "listParser (tokenize \"[ [ ] ] [ ] ]\") [] Map.empty returns ([LIST \"2\", LIST \"1\"], [], (Map.fromList [(\"0\", []), (\"1\", [LIST \"0\"]), (\"2\", [])]))" $ do
            listParser (tokenize "[ [ ] ] [ ] ]") [] Map.empty `shouldBe` ([LIST "2", LIST "1"], [], Map.fromList [("0", []), ("1", [LIST "0"]), ("2", [])])
        it "listParser (tokenize \"1 \" a string \" 2 ]\") [] Map.empty returns ([INT 2, STRING \"a string\", INT 1], [], Map.empty)" $ do
            listParser (tokenize "1 \" a string \" 2 ]") [] Map.empty `shouldBe` ([INT 2, STRING "a string", INT 1], [], Map.empty)
        it "listParser (tokenize \"1 { 2 } 3 ]\") [] Map.empty returns ([INT 3, CODEBLOCK \"0\", INT 1], [], (Map.fromList [(\"0\", [INT 2])]))" $ do
            listParser (tokenize "1 { 2 } 3 ]") [] Map.empty `shouldBe` ([INT 3, CODEBLOCK "0", INT 1], [], Map.fromList [("0", [INT 2])])

spec_stringParser :: Spec
spec_stringParser = do
    describe "stringParser tests:" $ do
        it "stringParser [\"a\", \"string\", \"] [] returns (STRING \"a string\", [])" $ do
            stringParser ["a", "string", "\""] [] `shouldBe` (STRING "a string", [])
        it "stringParser [\"a\", \"string\", \", \", \"b\", \"string\", \"] [] returns (STRING \"a string\", [\", \"b\", \"string\", \"])" $ do
            stringParser ["a", "string", "\"", "\"", "b", "string", "\""] [] `shouldBe` (STRING "a string", ["\"", "b", "string", "\""])

spec_typeParser :: Spec
spec_typeParser = do
    describe "typeParser tests:" $ do
        it "typeParser \"1\" returns INT 1" $ do
            typeParser "1" `shouldBe` INT 1
        it "typeParser \"1.5\" returns FLOAT 1.5" $ do
            typeParser "1.5" `shouldBe` FLOAT 1.5
        it "typeParser \"True\" returns BOOL True" $ do
            typeParser "True" `shouldBe` BOOL True
        it "typeParser \"+\" returns FUNC \"+\"" $ do
            typeParser "+" `shouldBe` FUNC "+"
        it "typeParser \"abc\" returns UNKNOWN \"abc\"" $ do
            typeParser "abc" `shouldBe` UNKNOWN "abc"

{-- offical tests. modified so it works with bprog2 and output is in format of a stack [...] -}

spec_testCompiler :: Spec
spec_testCompiler = do
    describe "official tests for non-error programs" $ do
        {-- literals -}
        it "literals test" $ do
            testCompiler "3" `shouldBe` "[3]"
        it "literals test" $ do
            testCompiler "121231324135634563456363567" `shouldBe` "[121231324135634563456363567]"
        it "literals test" $ do
            testCompiler "1.0" `shouldBe` "[1.0]"
        it "literals test" $ do
            testCompiler "0.0" `shouldBe` "[0.0]"
        it "literals test" $ do
            testCompiler "-1" `shouldBe` "[-1]"
        it "literals test" $ do
            testCompiler "-1.1" `shouldBe` "[-1.1]"
        it "literals test" $ do
            testCompiler "False" `shouldBe` "[False]"
        it "literals test" $ do
            testCompiler "True" `shouldBe` "[True]"
        it "literals test" $ do
            testCompiler "[ [ ] [ ] ]" `shouldBe` "[[[],[]]]"
        it "literals test" $ do
            testCompiler "[ False [ ] True [ 1 2 ] ]" `shouldBe` "[[False,[],True,[1,2]]]"
        it "literals test" $ do
            testCompiler "\" [ so { not if ] and } \"" `shouldBe` "[\"[ so { not if ] and }\"]"
        {-- quotation literals -}
        it "quotation literals test" $ do
            testCompiler "{ 20 10 + }" `shouldBe` "[{20 10 +}]"
        it "quotation literals test" $ do
            testCompiler "[ { + } { 10 + } { 20 10 + } ]" `shouldBe` "[[{+},{10 +},{20 10 +}]]"

        {-- simple arithmetic -}
        it "simple arithmetic test" $ do
            testCompiler "1 1 +" `shouldBe` "[2]"       
        it "simple arithmetic test" $ do
            testCompiler "10 20 *" `shouldBe` "[200]"
        it "simple arithmetic test" $ do
            testCompiler "20 2 div" `shouldBe` "[10]"
        it "simple arithmetic test" $ do
            testCompiler "20 2 /" `shouldBe` "[10.0]"

        {-- arithmetic with type coercion -}
        it "arithmetic with type coercion test" $ do
            testCompiler "1 1.0 +" `shouldBe` "[2.0]"       
        it "arithmetic with type coercion test" $ do
            testCompiler "10 20.0 *" `shouldBe` "[200.0]"
        it "arithmetic with type coercion test" $ do
            testCompiler "20 2.0 div" `shouldBe` "[10]"
        it "arithmetic with type coercion test" $ do
            testCompiler "20.0 2.0 div" `shouldBe` "[10]"

        {-- bool operations -}
        it "bool operations test" $ do
            testCompiler "False False &&" `shouldBe` "[False]"
        it "bool operations test" $ do
            testCompiler "False True ||" `shouldBe` "[True]"
        it "bool operations test" $ do
            testCompiler "False not" `shouldBe` "[True]"
        it "bool operations test" $ do
            testCompiler "True not" `shouldBe` "[False]"

        {-- comparisons -}
        it "comparisons test" $ do
            testCompiler "20 10 <" `shouldBe` "[False]"
        it "comparisons test" $ do
            testCompiler "20 10 >" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler "20 10.0 >" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler "20.0 20.0 >" `shouldBe` "[False]"
        it "comparisons test" $ do
            testCompiler "10 10 ==" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler "10 10.0 ==" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler "True True ==" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler "True 40 40 == ==" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler "\" abba \" \" abba \" ==" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler "[ ] [ ] ==" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler "[ 1 2 ] [ 1 2 ] ==" `shouldBe` "[True]"
        it "comparisons test" $ do
            testCompiler " [ [ ] ] [ [ ] ] ==" `shouldBe` "[True]"

        {-- stack operations -}
        it "stack operations test" $ do
            testCompiler "10 20 swap pop" `shouldBe` "[20]"
        it "stack operations test" $ do
            testCompiler "10 dup dup + swap pop" `shouldBe` "[20]"
        it "stack operations test" $ do
            testCompiler "10 20 swap dup + div" `shouldBe` "[1]"

        {-- length -}
        it "length test" $ do
            testCompiler "\" hello \" length" `shouldBe` "[5]"
        it "length test" $ do
            testCompiler "\" hello world \" length" `shouldBe` "[11]"
        it "length test" $ do
            testCompiler "[ 1 2 3 [ ] ] length" `shouldBe` "[4]"
        it "length test" $ do
            testCompiler "{ 10 20 + } length" `shouldBe` "[3]"

        {-- String parsing -}
        it "String parsing test" $ do
            testCompiler "\" 12 \" parseInteger" `shouldBe` "[12]"
        it "String parsing test" $ do
            testCompiler "\" 12.34 \" parseFloat" `shouldBe` "[12.34]"
        it "String parsing test" $ do
            testCompiler "\" adam bob charlie \" words" `shouldBe` "[[\"adam\",\"bob\",\"charlie\"]]"          

        {-- lists -}
        it "lists test" $ do
            testCompiler "[ 1 2 3 ]" `shouldBe` "[[1,2,3]]"
        it "lists test" $ do
            testCompiler "[ 1 \" bob \" ]" `shouldBe` "[[1,\"bob\"]]"
        it "lists test" $ do
            testCompiler "[ 1 2 ] empty" `shouldBe` "[False]"
        it "lists test" $ do
            testCompiler "[ ] empty" `shouldBe` "[True]"
        it "lists test" $ do
            testCompiler "[ 1 2 3 ] head" `shouldBe` "[1]"
        it "lists test" $ do
            testCompiler "[ 1 2 3 ] length" `shouldBe` "[3]"
        it "lists test" $ do
            testCompiler "[ 1 2 3 ] tail" `shouldBe` "[[2,3]]"
        it "lists test" $ do
            testCompiler "1 [ ] cons" `shouldBe` "[[1]]"
        it "lists test" $ do
            testCompiler "1 [ 2 3 ] cons" `shouldBe` "[[1,2,3]]"
        it "lists test" $ do
            testCompiler "[ 1 ] [ 2 3 ] append" `shouldBe` "[[1,2,3]]"
        it "lists test" $ do
            testCompiler "[ 1 2 ] [ ] append" `shouldBe` "[[1,2]]"
        it "lists test" $ do
            testCompiler "[ 1 ] [ 2 3 ] cons" `shouldBe` "[[[1],2,3]]"

        {-- list quotations -}
        it "lists quotations test" $ do
            testCompiler "[ 1 2 3 ] { 10 * } map" `shouldBe` "[[10,20,30]]"
        it "lists quotations test" $ do
            testCompiler "[ 1 2 3 ] { 1 + } map" `shouldBe` "[[2,3,4]]"
        it "lists quotations test" $ do
            testCompiler "[ 1 2 3 4 ] { dup 2 > { 10 * } { 2 * } if } map" `shouldBe` "[[2,4,30,40]]"
        it "lists quotations test" $ do
            testCompiler "[ 1 2 3 4 ] { 10 * } each + + +" `shouldBe` "[100]"
        it "lists quotations test" $ do
            testCompiler "[ 1 2 3 4 ] 0 { + } foldl" `shouldBe` "[10]"
        it "lists quotations test" $ do
            testCompiler "[ 2 5 ] 20 { div } foldl" `shouldBe` "[2]"
        {-- note no { } needed for 1 instruction code -}
        it "lists quotations test" $ do
            testCompiler "[ \" 1 \" \" 2 \" \" 3 \" ] { parseInteger } each [ ] cons cons cons" `shouldBe` "[[1,2,3]]"
        it "lists quotations test" $ do
            testCompiler "[ \" 1 \" \" 2 \" \" 3 \" ] parseInteger each [ ] 3 cons times" `shouldBe` "[[1,2,3]]"
        it "lists quotations test" $ do
            testCompiler "[ 1 2 3 4 ] 0 + foldl" `shouldBe` "[10]"
        it "lists quotations test" $ do
            testCompiler "[ 2 5 ] 20 div foldl" `shouldBe` "[2]"
        
        {-- assignments -}
        it "assignments quotations test" $ do
            testCompiler "age" `shouldBe` "[age]"
        it "assignments quotations test" $ do
            testCompiler "age 10 := age" `shouldBe` "[10]"
        it "assignments quotations test" $ do
            testCompiler "10 age swap := age" `shouldBe` "[10]"
        it "assignments quotations test" $ do
            testCompiler "[ 1 2 3 ] list swap := list" `shouldBe` "[[1,2,3]]"
        it "assignments quotations test" $ do
            testCompiler "age 20 := [ 10 age ]" `shouldBe` "[[10,20]]"

        it "assignments quotations test" $ do
            testCompiler "inc { 1 + } fun 1 inc" `shouldBe` "[2]"
        it "assignments quotations test" $ do
            testCompiler "mul10 { 10 * } fun inc { 1 + } fun 10 inc mul10" `shouldBe` "[110]"

        {-- quotations -}
        it "quotations test" $ do
            testCompiler "{ 20 10 + } exec" `shouldBe` "[30]"
        it "quotations test" $ do
            testCompiler "10 { 20 + } exec" `shouldBe` "[30]"
        it "quotations test" $ do
            testCompiler "10 20 { + } exec" `shouldBe` "[30]"
        it "quotations test" $ do
            testCompiler "{ { 10 20 + } exec } exec" `shouldBe` "[30]"
        it "quotations test" $ do
            testCompiler "{ { 10 20 + } exec 20 + } exec" `shouldBe` "[50]"

        {-- if -}
        it "if test" $ do
            testCompiler "True { 20 } { } if" `shouldBe` "[20]"
        it "if test" $ do
            testCompiler "True { 20 10 + } { 3 } if" `shouldBe` "[30]"
        it "if test" $ do
            testCompiler "10 5 5 == { 10 + } { 100 + } if" `shouldBe` "[20]"
        it "if test" $ do
            testCompiler "False { } { 45 } if" `shouldBe` "[45]"
        it "if test" $ do
            testCompiler "True { False { 50 } { 100 } if } { 30 } if" `shouldBe` "[100]"

        {-- if without quotation, more ergonomic expressions -}
        it "if without quotation test" $ do
            testCompiler "True 20 { } if" `shouldBe` "[20]"
        it "if without quotation test" $ do
            testCompiler "True { 20 10 + } 3 if" `shouldBe` "[30]"
        it "if without quotation test" $ do
            testCompiler "10 10 5 5 == + { 100 + } if" `shouldBe` "[20]"
        it "if without quotation test" $ do
            testCompiler "False { } 45 if" `shouldBe` "[45]"
        it "if without quotation test" $ do
            testCompiler "True { False 50 100 if } 30 if" `shouldBe` "[100]"

        {-- times -}
        it "times test" $ do
            testCompiler "1 { 100 50 + } times" `shouldBe` "[150]"
        it "times test" $ do
            testCompiler "5 { 1 } times [ ] 5 { cons } times 0 { + } foldl" `shouldBe` "[5]"
        it "times test" $ do
            testCompiler "5   1   times [ ] 5   cons   times 0   +   foldl" `shouldBe` "[5]"
        it "times test" $ do
            testCompiler "5 { 10 } times + + + +" `shouldBe` "[50]"
        it "times test" $ do
            testCompiler "5 10 times 4 + times" `shouldBe` "[50]"

        {-- loop -}
        it "loop test" $ do
            testCompiler "1 { dup 4 > } { dup 1 + } loop [ ] 5 { cons } times" `shouldBe` "[[1,2,3,4,5]]"
        it "loop test" $ do
            testCompiler "1 { dup 4 > } { dup 1 + } loop [ ] 5   cons   times" `shouldBe` "[[1,2,3,4,5]]"
        it "loop test" $ do
            testCompiler "[ 1 ] { dup length 9 > }  { dup head 1 + swap cons } loop" `shouldBe` "[[10,9,8,7,6,5,4,3,2,1]]"

        {-- other -}
        it "other test" $ do
            testCompiler "odd { dup 2 div swap 2 / == False True if } fun \
        \ 2 odd" `shouldBe` "[False]"
        
        it "other test" $ do
            testCompiler "odd { dup 2 div swap 2 / == False True if } fun \
        \ 3 odd" `shouldBe` "[True]"
        
        it "other test" $ do
            testCompiler "toList { [ ] swap cons times } fun \
        \ 1 2 3 4 \
        \ 4 toList" `shouldBe` "[[1,2,3,4]]"
        
        it "other test" $ do
            testCompiler "gen1toNum { max swap := 1 { dup max > } { dup 1 + } loop } fun \
        \ 3 gen1toNum + + +" `shouldBe` "[10]"

        it "other test" $ do
            testCompiler "odd { dup 2 div swap 2 / == False True if } fun \
        \ toList { [ ] swap cons times } fun \
        \ gen1toNum { max swap := 1 { dup max > } { dup 1 + } loop } fun \
        \ 4 gen1toNum 5 toList odd map" `shouldBe` "[[True,False,True,False,True]]"
