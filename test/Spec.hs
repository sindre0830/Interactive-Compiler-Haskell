-- foreign modules
import Test.Hspec ( Spec, hspec, shouldBe, it, describe )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
-- local modules
import Dictionary
import Parsing
import Stack
import Convert
import Compiler
-- | Main testing program.
main :: IO ()
main = do
    hspec $ do
        -- module compiler
        spec_funcAddition
        spec_funcSubtraction
        spec_funcMultiplication
        spec_funcDivisionFloat
        spec_funcDivisionInteger
        spec_funcAND
        spec_funcOR
        spec_funcNOT
        spec_funcEqual
        spec_funcLess
        spec_funcGreater
        spec_funcPop
        spec_funcDup
        spec_funcSwap
        spec_funcParseInteger
        spec_funcParseFloat
        spec_funcWords
        spec_funcEmpty
        spec_funcHead
        spec_funcTail
        spec_funcCons
        spec_funcAppend
        spec_funcLength
        spec_funcExec
        spec_funcIf
        spec_funcMap
        spec_funcEach
        spec_funcFoldl
        spec_funcTimes
        spec_funcLoop
        spec_funcSetVariable
        spec_funcSetFunction
        spec_funcPrint
        -- module Parsing
        --spec_parseInput
        spec_tokenize
        spec_parser
        spec_codeBlockParser
        spec_listParser
        spec_stringParser
        spec_typeParser
        -- module Stack
        spec_generateObjectAddress
        spec_getValidAddress
        spec_updateObject
        spec_allocateObject
        spec_deallocateObject
        spec_printableStack
        spec_formatStack

-- module compiler

spec_funcAddition :: Spec
spec_funcAddition = do
    describe "funcAddition tests:" $ do
        it "printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[3]\"" $ do
            printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[3]"
        it "printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) returns \"[4.0]\"" $ do
            printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) `shouldBe` "[4.0]"
        it "printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) returns \"[3.5]\"" $ do
            printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) `shouldBe` "[3.5]"
        it "printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[3.5]\"" $ do
            printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[3.5]"
        it "printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcAddition ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcSubtraction :: Spec
spec_funcSubtraction = do
    describe "funcSubtraction tests:" $ do
        it "printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[-1]\"" $ do
            printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[-1]"
        it "printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) returns \"[-1.0]\"" $ do
            printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) `shouldBe` "[-1.0]"
        it "printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) returns \"[-1.5]\"" $ do
            printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) `shouldBe` "[-1.5]"
        it "printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[-0.5]\"" $ do
            printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[-0.5]"
        it "printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcSubtraction ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcMultiplication :: Spec
spec_funcMultiplication = do
    describe "funcMultiplication tests:" $ do
        it "printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[2]\"" $ do
            printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[2]"
        it "printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) returns \"[3.75]\"" $ do
            printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5], None)) `shouldBe` "[3.75]"
        it "printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) returns \"[2.5]\"" $ do
            printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 1], None)) `shouldBe` "[2.5]"
        it "printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[3.0]\"" $ do
            printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[3.0]"
        it "printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcMultiplication ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcDivisionFloat :: Spec
spec_funcDivisionFloat = do
    describe "funcDivisionFloat tests:" $ do
        it "printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 10], None)) returns \"[5.0]\"" $ do
            printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 10], None)) `shouldBe` "[5.0]"
        it "printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, FLOAT 10.0], None)) returns \"[5.0]\"" $ do
            printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, FLOAT 10.0], None)) `shouldBe` "[5.0]"
        it "printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, INT 10], None)) returns \"[5.0]\"" $ do
            printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, INT 10], None)) `shouldBe` "[5.0]"
        it "printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 10.0], None)) returns \"[5.0]\"" $ do
            printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 10.0], None)) `shouldBe` "[5.0]"
        it "printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcDivisionFloat ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcDivisionInteger :: Spec
spec_funcDivisionInteger = do
    describe "funcDivisionInteger tests:" $ do
        it "printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 10], None)) returns \"[5]\"" $ do
            printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 10], None)) `shouldBe` "[5]"
        it "printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, FLOAT 10.0], None)) returns \"[5]\"" $ do
            printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, FLOAT 10.0], None)) `shouldBe` "[5]"
        it "printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, INT 10], None)) returns \"[5]\"" $ do
            printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.0, INT 10], None)) `shouldBe` "[5]"
        it "printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 10.0], None)) returns \"[5]\"" $ do
            printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 10.0], None)) `shouldBe` "[5]"
        it "printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcDivisionInteger ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcAND :: Spec
spec_funcAND = do
    describe "funcAND tests:" $ do
        it "printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) returns \"[True]\"" $ do
            printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) returns \"[False]\"" $ do
            printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) returns \"[False]\"" $ do
            printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, INT 10], None)) returns \"[ExpectedBool]\"" $ do
            printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [BOOL False, INT 10], None)) `shouldBe` "[ExpectedBool]"
        it "printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcAND ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcOR :: Spec
spec_funcOR = do
    describe "funcOR tests:" $ do
        it "printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) returns \"[True]\"" $ do
            printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) returns \"[False]\"" $ do
            printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) returns \"[True]\"" $ do
            printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, INT 10], None)) returns \"[ExpectedBool]\"" $ do
            printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [BOOL False, INT 10], None)) `shouldBe` "[ExpectedBool]"
        it "printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcOR ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcNOT :: Spec
spec_funcNOT = do
    describe "funcNOT tests:" $ do
        it "printableStack (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) returns \"[False]\"" $ do
            printableStack (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [BOOL False], None)) returns \"[True]\"" $ do
            printableStack (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [BOOL False], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) returns \"[ExpectedBool]\"" $ do
            printableStack (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) `shouldBe` "[ExpectedBool]"
        it "printableStack (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcNOT ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcEqual :: Spec
spec_funcEqual = do
    describe "funcEqual tests:" $ do
        it "printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) returns \"[True]\"" $ do
            printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL True, BOOL True], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) returns \"[True]\"" $ do
            printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL False], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) returns \"[False]\"" $ do
            printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [BOOL False, BOOL True], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) returns \"[True]\"" $ do
            printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcEqual ([], Map.fromList [(\"0\", [INT 1]), (\"1\", [INT 1])], Map.empty, Map.empty, [LIST \"0\", LIST \"1\"], None)) returns \"[True]\"" $ do
            printableStack (evalState funcEqual ([], Map.fromList [("0", [INT 1]), ("1", [INT 1])], Map.empty, Map.empty, [LIST "0", LIST "1"], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcEqual ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcLess :: Spec
spec_funcLess = do
    describe "funcLess tests:" $ do
        it "printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[True]\"" $ do
            printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 2.5], None)) returns \"[False]\"" $ do
            printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 2.5], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 10], None)) returns \"[False]\"" $ do
            printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 10], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[True]\"" $ do
            printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcLess ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcGreater :: Spec
spec_funcGreater = do
    describe "funcGreater tests:" $ do
        it "printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) returns \"[False]\"" $ do
            printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, INT 1], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 2.5], None)) returns \"[False]\"" $ do
            printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, FLOAT 2.5], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 10], None)) returns \"[True]\"" $ do
            printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [FLOAT 2.5, INT 10], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) returns \"[False]\"" $ do
            printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, FLOAT 1.5], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2], None)) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcGreater ([], Map.empty, Map.empty, Map.empty, [INT 2, BOOL True], None)) `shouldBe` "[ExpectedNumber]"

spec_funcPop :: Spec
spec_funcPop = do
    describe "funcPop tests:" $ do
        it "printableStack (evalState funcPop ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) returns \"[]\"" $ do
            printableStack (evalState funcPop ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) `shouldBe` "[]"
        it "printableStack (evalState funcPop ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcPop ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcDup :: Spec
spec_funcDup = do
    describe "funcDup tests:" $ do
        it "printableStack (evalState funcDup ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) returns \"[True, True]\"" $ do
            printableStack (evalState funcDup ([], Map.empty, Map.empty, Map.empty, [BOOL True], None)) `shouldBe` "[True, True]"
        it "printableStack (evalState funcDup ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcDup ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcSwap :: Spec
spec_funcSwap = do
    describe "funcSwap tests:" $ do
        it "printableStack (evalState funcSwap ([], Map.empty, Map.empty, Map.empty, [BOOL True, INT 5], None)) returns \"[True, 5]\"" $ do
            printableStack (evalState funcSwap ([], Map.empty, Map.empty, Map.empty, [BOOL True, INT 5], None)) `shouldBe` "[True, 5]"
        it "printableStack (evalState funcSwap ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcSwap ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcParseInteger :: Spec
spec_funcParseInteger = do
    describe "funcParseInteger tests:" $ do
        it "printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING \"100\"], None)) returns \"[100]\"" $ do
            printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING "100"], None)) `shouldBe` "[100]"
        it "printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING \"100.0\"], None)) returns \"[ExpectedStringOfInteger]\"" $ do
            printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING "100.0"], None)) `shouldBe` "[ExpectedStringOfInteger]"
        it "printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING \"abc\"], None)) returns \"[ExpectedStringOfInteger]\"" $ do
            printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [STRING "abc"], None)) `shouldBe` "[ExpectedStringOfInteger]"
        it "printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [INT 5], None)) returns \"[ExpectedString]\"" $ do
            printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [INT 5], None)) `shouldBe` "[ExpectedString]"
        it "printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcParseInteger ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcParseFloat :: Spec
spec_funcParseFloat = do
    describe "funcParseFloat tests:" $ do
        it "printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING \"100.5\"], None)) returns \"[100.5]\"" $ do
            printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING "100.5"], None)) `shouldBe` "[100.5]"
        it "printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING \"100\"], None)) returns \"[100.0]\"" $ do
            printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING "100"], None)) `shouldBe` "[100.0]"
        it "printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING \"abc\"], None)) returns \"[ExpectedStringOfFloat]\"" $ do
            printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [STRING "abc"], None)) `shouldBe` "[ExpectedStringOfFloat]"
        it "printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedString]\"" $ do
            printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedString]"
        it "printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcParseFloat ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcWords :: Spec
spec_funcWords = do
    describe "funcWords tests:" $ do
        it "printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING \"a b c\"], None)) returns \"[\"a\", \"b\", \"c\"]\"" $ do
            printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING "a b c"], None)) `shouldBe` "[\"a\", \"b\", \"c\"]"
        it "printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING \"a\"], None)) returns \"[\"a\"]\"" $ do
            printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING "a"], None)) `shouldBe` "[\"a\"]"
        it "printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING \"\"], None)) returns \"[]\"" $ do
            printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [STRING ""], None)) `shouldBe` "[]"
        it "printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedString]\"" $ do
            printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedString]"
        it "printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcWords ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcEmpty :: Spec
spec_funcEmpty = do
    describe "funcEmpty tests:" $ do
        it "printableStack (evalState funcEmpty ([], Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[False]\"" $ do
            printableStack (evalState funcEmpty ([], Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[False]"
        it "printableStack (evalState funcEmpty ([], Map.fromList [(\"0\", [])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[True]\"" $ do
            printableStack (evalState funcEmpty ([], Map.fromList [("0", [])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[True]"
        it "printableStack (evalState funcEmpty ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcEmpty ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcEmpty ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcEmpty ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcHead :: Spec
spec_funcHead = do
    describe "funcHead tests:" $ do
        it "printableStack (evalState funcHead ([], Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[1]\"" $ do
            printableStack (evalState funcHead ([], Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[1]"
        it "printableStack (evalState funcHead ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcHead ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcHead ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcHead ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcTail :: Spec
spec_funcTail = do
    describe "funcTail tests:" $ do
        it "printableStack (evalState funcTail ([], Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[[2, 3]]\"" $ do
            printableStack (evalState funcTail ([], Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[[2, 3]]"
        it "printableStack (evalState funcTail ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcTail ([], Map.empty, Map.empty, Map.empty, [INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcTail ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcTail ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcCons :: Spec
spec_funcCons = do
    describe "funcCons tests:" $ do
        it "printableStack (evalState funcCons ([], Map.fromList [(\"0\", [INT 2, INT 3])], Map.empty, Map.empty, [LIST \"0\", INT 1], None)) returns \"[[1, 2, 3]]\"" $ do
            printableStack (evalState funcCons ([], Map.fromList [("0", [INT 2, INT 3])], Map.empty, Map.empty, [LIST "0", INT 1], None)) `shouldBe` "[[1, 2, 3]]"
        it "printableStack (evalState funcCons ([], Map.fromList [(\"0\", []), (\"1\", [])], Map.empty, Map.empty, [LIST \"1\", LIST \"0\"], None)) returns \"[[[]]]\"" $ do
            printableStack (evalState funcCons ([], Map.fromList [("0", []), ("1", [])], Map.empty, Map.empty, [LIST "1", LIST "0"], None)) `shouldBe` "[[[]]]"
        it "printableStack (evalState funcCons ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcCons ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcCons ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcCons ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcAppend :: Spec
spec_funcAppend = do
    describe "funcAppend tests:" $ do
        it "printableStack (evalState funcAppend ([], Map.fromList [(\"0\", [INT 2, INT 3]), (\"1\", [INT 1])], Map.empty, Map.empty, [LIST \"0\", LIST \"1\"], None)) returns \"[[1, 2, 3]]\"" $ do
            printableStack (evalState funcAppend ([], Map.fromList [("0", [INT 2, INT 3]), ("1", [INT 1])], Map.empty, Map.empty, [LIST "0", LIST "1"], None)) `shouldBe` "[[1, 2, 3]]"
        it "printableStack (evalState funcAppend ([], Map.fromList [(\"0\", []), (\"1\", [])], Map.empty, Map.empty, [LIST \"1\", LIST \"0\"], None)) returns \"[[]]\"" $ do
            printableStack (evalState funcAppend ([], Map.fromList [("0", []), ("1", [])], Map.empty, Map.empty, [LIST "1", LIST "0"], None)) `shouldBe` "[[]]"
        it "printableStack (evalState funcAppend ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcAppend ([], Map.empty, Map.empty, Map.empty, [INT 10, INT 10], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcAppend ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcAppend ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcLength :: Spec
spec_funcLength = do
    describe "funcLength tests:" $ do
        it "printableStack (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [STRING \"a b c\"], None)) returns \"[5]\"" $ do
            printableStack (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [STRING "a b c"], None)) `shouldBe` "[5]"
        it "printableStack (evalState funcLength ([], Map.fromList [(\"0\", [INT 1, INT 2])], Map.empty, Map.empty, [LIST \"0\"], None)) returns \"[2]\"" $ do
            printableStack (evalState funcLength ([], Map.fromList [("0", [INT 1, INT 2])], Map.empty, Map.empty, [LIST "0"], None)) `shouldBe` "[2]"
        it "printableStack (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [STRING \"\"], None)) returns \"[0]\"" $ do
            printableStack (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [STRING ""], None)) `shouldBe` "[0]"
        it "printableStack (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcLength ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcExec :: Spec
spec_funcExec = do
    describe "funcExec tests:" $ do
        it "printableStack (evalState funcExec ([], Map.fromList [(\"0\", [INT 1, INT 2, INT 3, FUNC \"+\"])], Map.empty, Map.empty, [CODEBLOCK \"0\"], None)) returns \"[]\"" $ do
            printableStack (evalState funcExec ([], Map.fromList [("0", [INT 1, INT 2, INT 3, FUNC "+"])], Map.empty, Map.empty, [CODEBLOCK "0"], None)) `shouldBe` "[]"
        it "printableStack (evalState funcExec ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcExec ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcExec ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcExec ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcIf :: Spec
spec_funcIf = do
    describe "funcIf tests:" $ do
        it "printableStack (evalState funcIf ([], Map.fromList [(\"0\", [FUNC \"+\"]), (\"1\", [FUNC \"*\"])], Map.empty, Map.empty, [CODEBLOCK \"1\", CODEBLOCK \"0\", BOOL True], None)) returns \"[]\"" $ do
            printableStack (evalState funcIf ([], Map.fromList [("0", [FUNC "+"]), ("1", [FUNC "*"])], Map.empty, Map.empty, [CODEBLOCK "1", CODEBLOCK "0", BOOL True], None)) `shouldBe` "[]"
        it "printableStack (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedBool]\"" $ do
            printableStack (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedBool]"
        it "printableStack (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, BOOL True], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, BOOL True], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcIf ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcMap :: Spec
spec_funcMap = do
    describe "funcMap tests:" $ do
        it "printableStack (evalState funcMap ([], Map.fromList [(\"0\", [INT 10, FUNC \"*\"]), (\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [CODEBLOCK \"0\", LIST \"1\"], None)) returns \"[[30, 20, 10]]\"" $ do
            printableStack (evalState funcMap ([], Map.fromList [("0", [INT 10, FUNC "*"]), ("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [CODEBLOCK "0", LIST "1"], None)) `shouldBe` "[[30, 20, 10]]"
        it "printableStack (evalState funcMap ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcMap ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcMap ([], Map.fromList [(\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, LIST \"0\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcMap ([], Map.fromList [("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, LIST "0"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcMap ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcMap ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcEach :: Spec
spec_funcEach = do
    describe "funcEach tests:" $ do
        it "printableStack (evalState funcEach ([], Map.fromList [(\"0\", [INT 10, FUNC \"*\"]), (\"1\", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [CODEBLOCK \"0\", LIST \"1\"], None)) returns \"[10, 20, 30]\"" $ do
            printableStack (evalState funcEach ([], Map.fromList [("0", [INT 10, FUNC "*"]), ("1", [INT 1, INT 2, INT 3])], Map.empty, Map.empty, [CODEBLOCK "0", LIST "1"], None)) `shouldBe` "[10, 20, 30]"
        it "printableStack (evalState funcEach ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcEach ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcEach ([], Map.fromList [(\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, LIST \"0\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcEach ([], Map.fromList [("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, LIST "0"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcEach ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcEach ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcFoldl :: Spec
spec_funcFoldl = do
    describe "funcFoldl tests:" $ do
        it "printableStack (evalState funcFoldl ([], Map.fromList [(\"0\", [FUNC \"+\"]), (\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [CODEBLOCK \"0\", INT 0, LIST \"1\"], None)) returns \"[6]\"" $ do
            printableStack (evalState funcFoldl ([], Map.fromList [("0", [FUNC "+"]), ("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [CODEBLOCK "0", INT 0, LIST "1"], None)) `shouldBe` "[6]"
        it "printableStack (evalState funcFoldl ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcFoldl ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcFoldl ([], Map.fromList [(\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, FUNC \"+\", LIST \"0\"], None)) returns \"[InvalidType]\"" $ do
            printableStack (evalState funcFoldl ([], Map.fromList [("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, FUNC "+", LIST "0"], None)) `shouldBe` "[InvalidType]"
        it "printableStack (evalState funcFoldl ([], Map.fromList [(\"1\", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, INT 0, LIST \"0\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcFoldl ([], Map.fromList [("1", [INT 3, INT 2, INT 1])], Map.empty, Map.empty, [FLOAT 5.0, INT 0, LIST "0"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcFoldl ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcFoldl ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcTimes :: Spec
spec_funcTimes = do
    describe "funcTimes tests:" $ do
        it "printableStack (evalState funcTimes ([], Map.fromList [(\"0\", [INT 10, INT 10, FUNC \"*\"])], Map.empty, Map.empty, [CODEBLOCK \"0\", INT 4], None)) returns \"[]\"" $ do
            printableStack (evalState funcTimes ([], Map.fromList [("0", [INT 10, INT 10, FUNC "*"])], Map.empty, Map.empty, [CODEBLOCK "0", INT 4], None)) `shouldBe` "[]"
        it "printableStack (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedPositiveInteger]\"" $ do
            printableStack (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedPositiveInteger]"
        it "printableStack (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, INT 1], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, INT 1], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcTimes ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcLoop :: Spec
spec_funcLoop = do
    describe "funcLoop tests:" $ do
        it "printableStack (evalState funcLoop ([], Map.fromList [(\"0\", [FUNC \"dup\", INT 4, FUNC \"dup\"]), (\"1\", [FUNC \"dup\", INT 1, FUNC \"+\"])], Map.empty, Map.empty, [CODEBLOCK \"1\", CODEBLOCK \"0\", INT 1], None)) returns \"[]\"" $ do
            printableStack (evalState funcLoop ([], Map.fromList [("0", [FUNC "dup", INT 4, FUNC ">"]), ("1", [FUNC "dup", INT 1, FUNC "+"])], Map.empty, Map.empty, [CODEBLOCK "1", CODEBLOCK "0", INT 1], None)) `shouldBe` "[]"
        it "printableStack (evalState funcLoop ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcLoop ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcLoop ([], Map.fromList [(\"0\", [FUNC \"dup\", INT 4, FUNC \"dup\"])], Map.empty, Map.empty, [FLOAT 5.0, CODEBLOCK \"0\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcLoop ([], Map.fromList [("0", [FUNC "dup", INT 4, FUNC ">"])], Map.empty, Map.empty, [FLOAT 5.0, CODEBLOCK "0"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcLoop ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcLoop ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcSetVariable :: Spec
spec_funcSetVariable = do
    describe "funcSetVariable tests:" $ do
        it "printableStack (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, UNKNOWN \"a\"], None)) returns \"[]\"" $ do
            printableStack (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, UNKNOWN "a"], None)) `shouldBe` "[]"
        it "printableStack (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedUnknown]\"" $ do
            printableStack (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedUnknown]"
        it "printableStack (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcSetVariable ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcSetFunction :: Spec
spec_funcSetFunction = do
    describe "funcSetFunction tests:" $ do
        it "printableStack (evalState funcSetFunction ([], Map.fromList [(\"0\", [FUNC \"+\"])], Map.empty, Map.empty, [CODEBLOCK \"0\", UNKNOWN \"a\"], None)) returns \"[]\"" $ do
            printableStack (evalState funcSetFunction ([], Map.fromList [("0", [FUNC "+"])], Map.empty, Map.empty, [CODEBLOCK "0", UNKNOWN "a"], None)) `shouldBe` "[]"
        it "printableStack (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) returns \"[ExpectedUnknown]\"" $ do
            printableStack (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, FLOAT 5.0], None)) `shouldBe` "[ExpectedUnknown]"
        it "printableStack (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, UNKNOWN \"a\"], None)) returns \"[ExpectedCodeblock]\"" $ do
            printableStack (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0, UNKNOWN "a"], None)) `shouldBe` "[ExpectedCodeblock]"
        it "printableStack (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcSetFunction ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"

spec_funcPrint :: Spec
spec_funcPrint = do
    describe "funcPrint tests:" $ do
        it "printableStack (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [STRING \"abc\"], None)) returns \"[Should have printed: abc]\"" $ do
            printableStack (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [STRING "abc"], None)) `shouldBe` "[Should have printed: abc]"
        it "printableStack (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) returns \"[ExpectedString]\"" $ do
            printableStack (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [FLOAT 5.0], None)) `shouldBe` "[ExpectedString]"
        it "printableStack (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcPrint ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[InvalidParameterAmount]"
-- module Parsing

-- spec_parseInput :: Spec
-- spec_parseInput = do
--     describe "parseInput tests:" $ do
--         it "printableStack (evalState (parseInput \"1 2 +\") ([], Map.empty, Map.empty, Map.empty, [], None)) returns \"[1, 2, +]\"" $ do
--             printableStack (evalState (parseInput "1 2 +") ([], Map.empty, Map.empty, Map.empty, [], None)) `shouldBe` "[1, 2, +]"

spec_tokenize :: Spec
spec_tokenize = do
    describe "tokenize tests:" $ do
        it "tokenize \"1 2 +\" returns [\"1\", \"2\", \"+\"]" $ do
            tokenize "1 2 +" `shouldBe` ["1", "2", "+"]

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

-- module Stack

spec_generateObjectAddress :: Spec
spec_generateObjectAddress = do
    describe "generateObjectAddress tests:" $ do
        it "generateObjectAddress (Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", []), (\"1\", [])]) returns \"4\"" $ do
            generateObjectAddress (Map.fromList [("0", []), ("3", []), ("2", []), ("5", []), ("1", [])]) `shouldBe` "4"
        it "generateObjectAddress Map.empty returns \"0\"" $ do
            generateObjectAddress Map.empty `shouldBe` "0"

spec_getValidAddress :: Spec
spec_getValidAddress = do
    describe "getValidAddress tests:" $ do
        it "getValidAddress (Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", []), (\"1\", [])]) 0 returns 4" $ do
            getValidAddress (Map.fromList [("0", []), ("3", []), ("2", []), ("5", []), ("1", [])]) 0 `shouldBe` 4
        it "getValidAddress Map.empty 0 returns 0" $ do
            getValidAddress Map.empty 0 `shouldBe` 0

spec_updateObject :: Spec
spec_updateObject = do
    describe "updateObject tests:" $ do
        it "updateObject \"0\" [INT 1, INT 2] (Map.fromList [(\"0\", [INT 1])]) returns Map.fromList [(\"0\", [INT 1, INT 2])]" $ do
            updateObject "0" [INT 1, INT 2] (Map.fromList [("0", [INT 1])]) `shouldBe` Map.fromList [("0", [INT 1, INT 2])]

spec_allocateObject :: Spec
spec_allocateObject = do
    describe "allocateObject tests:" $ do
        it "allocateObject [INT 1] (Map.fromList [(\"0\", []), (\"2\", [])]) returns (Map.fromList [(\"0\", []), (\"2\", []), (\"1\", [INT 1])], \"1\")" $ do
            allocateObject [INT 1] (Map.fromList [("0", []), ("2", [])]) `shouldBe` (Map.fromList [("0", []), ("2", []), ("1", [INT 1])], "1")

spec_deallocateObject :: Spec
spec_deallocateObject = do
    describe "deallocateObject tests:" $ do
        it "deallocateObject (LIST \"3\") (Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", [])]) returns Map.fromList [(\"0\", []), (\"2\", []), (\"5\", [])]" $ do
            deallocateObject (LIST "3") (Map.fromList [("0", []), ("3", []), ("2", []), ("5", [])]) `shouldBe` Map.fromList [("0", []), ("2", []), ("5", [])]
        it "deallocateObject (INT 1) Map.empty returns Map.empty" $ do
            deallocateObject (INT 1) Map.empty `shouldBe` Map.empty
        it "deallocateObject (INT 1) (Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", [])]) returns Map.fromList [(\"0\", []), (\"3\", []), (\"2\", []), (\"5\", [])]" $ do
            deallocateObject (INT 1) (Map.fromList [("0", []), ("3", []), ("2", []), ("5", [])]) `shouldBe` Map.fromList [("0", []), ("3", []), ("2", []), ("5", [])]

spec_printableStack :: Spec
spec_printableStack = do
    describe "printableStack tests:" $ do
        it "printableStack ([], Map.empty, Map.empty, Map.empty, [INT 2, STRING \"a string\", INT 1], None) returns \"[1, \"a string\", 2\"]" $ do
            printableStack ([], Map.empty, Map.empty, Map.empty, [INT 2, STRING "a string", INT 1], None) `shouldBe` "[1, \"a string\", 2]"

spec_formatStack :: Spec
spec_formatStack = do
    describe "formatStack tests:" $ do
        it "formatStack [INT 2, STRING \"a string\", INT 1] \", \" Map.empty returns \"2, \"a string\", 1\"" $ do
            formatStack [INT 2, STRING "a string", INT 1] ", " Map.empty `shouldBe` "2, \"a string\", 1"
