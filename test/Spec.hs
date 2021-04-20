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
        spec_funcAND
        spec_funcEqual
        spec_funcEmpty
        spec_funcHead
        spec_funcTail
        spec_funcCons
        spec_funcAppend
        -- module Parsing
        spec_parseInput
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
        it "printableStack (evalState funcAddition (Map.empty, Map.empty, [INT 2, INT 1])) returns \"[3]\"" $ do
            printableStack (evalState funcAddition (Map.empty, Map.empty, [INT 2, INT 1])) `shouldBe` "[3]"
        it "printableStack (evalState funcAddition (Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5])) returns \"[4.0]\"" $ do
            printableStack (evalState funcAddition (Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5])) `shouldBe` "[4.0]"
        it "printableStack (evalState funcAddition (Map.empty, Map.empty, [FLOAT 2.5, INT 1])) returns \"[3.5]\"" $ do
            printableStack (evalState funcAddition (Map.empty, Map.empty, [FLOAT 2.5, INT 1])) `shouldBe` "[3.5]"
        it "printableStack (evalState funcAddition (Map.empty, Map.empty, [INT 2, FLOAT 1.5])) returns \"[3.5]\"" $ do
            printableStack (evalState funcAddition (Map.empty, Map.empty, [INT 2, FLOAT 1.5])) `shouldBe` "[3.5]"
        it "printableStack (evalState funcAddition (Map.empty, Map.empty, [INT 2])) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcAddition (Map.empty, Map.empty, [INT 2])) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcAddition (Map.empty, Map.empty, [INT 2, BOOL True])) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcAddition (Map.empty, Map.empty, [INT 2, BOOL True])) `shouldBe` "[ExpectedNumber]"

spec_funcSubtraction :: Spec
spec_funcSubtraction = do
    describe "funcSubtraction tests:" $ do
        it "printableStack (evalState funcSubtraction (Map.empty, Map.empty, [INT 2, INT 1])) returns \"[-1]\"" $ do
            printableStack (evalState funcSubtraction (Map.empty, Map.empty, [INT 2, INT 1])) `shouldBe` "[-1]"
        it "printableStack (evalState funcSubtraction (Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5])) returns \"[-1.0]\"" $ do
            printableStack (evalState funcSubtraction (Map.empty, Map.empty, [FLOAT 2.5, FLOAT 1.5])) `shouldBe` "[-1.0]"
        it "printableStack (evalState funcSubtraction (Map.empty, Map.empty, [FLOAT 2.5, INT 1])) returns \"[-1.5]\"" $ do
            printableStack (evalState funcSubtraction (Map.empty, Map.empty, [FLOAT 2.5, INT 1])) `shouldBe` "[-1.5]"
        it "printableStack (evalState funcSubtraction (Map.empty, Map.empty, [INT 2, FLOAT 1.5])) returns \"[-0.5]\"" $ do
            printableStack (evalState funcSubtraction (Map.empty, Map.empty, [INT 2, FLOAT 1.5])) `shouldBe` "[-0.5]"
        it "printableStack (evalState funcSubtraction (Map.empty, Map.empty, [INT 2])) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcSubtraction (Map.empty, Map.empty, [INT 2])) `shouldBe` "[InvalidParameterAmount]"
        it "printableStack (evalState funcSubtraction (Map.empty, Map.empty, [INT 2, BOOL True])) returns \"[ExpectedNumber]\"" $ do
            printableStack (evalState funcSubtraction (Map.empty, Map.empty, [INT 2, BOOL True])) `shouldBe` "[ExpectedNumber]"

spec_funcAND :: Spec
spec_funcAND = do
    describe "funcAND tests:" $ do
        it "printableStack (evalState funcAND (Map.empty, Map.empty, [BOOL True, BOOL True])) returns \"[True]\"" $ do
            printableStack (evalState funcAND (Map.empty, Map.empty, [BOOL True, BOOL True])) `shouldBe` "[True]"
        it "printableStack (evalState funcAND (Map.empty, Map.empty, [BOOL False, BOOL False])) returns \"[False]\"" $ do
            printableStack (evalState funcAND (Map.empty, Map.empty, [BOOL False, BOOL False])) `shouldBe` "[False]"
        it "printableStack (evalState funcAND (Map.empty, Map.empty, [BOOL False, BOOL True])) returns \"[False]\"" $ do
            printableStack (evalState funcAND (Map.empty, Map.empty, [BOOL False, BOOL True])) `shouldBe` "[False]"
        it "printableStack (evalState funcAND (Map.empty, Map.empty, [BOOL False, INT 10])) returns \"[ExpectedBool]\"" $ do
            printableStack (evalState funcAND (Map.empty, Map.empty, [BOOL False, INT 10])) `shouldBe` "[ExpectedBool]"
        it "printableStack (evalState funcAND (Map.empty, Map.empty, [])) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcAND (Map.empty, Map.empty, [])) `shouldBe` "[InvalidParameterAmount]"

spec_funcEqual :: Spec
spec_funcEqual = do
    describe "funcEqual tests:" $ do
        it "printableStack (evalState funcEqual (Map.empty, Map.empty, [BOOL True, BOOL True])) returns \"[True]\"" $ do
            printableStack (evalState funcEqual (Map.empty, Map.empty, [BOOL True, BOOL True])) `shouldBe` "[True]"
        it "printableStack (evalState funcEqual (Map.empty, Map.empty, [BOOL False, BOOL False])) returns \"[True]\"" $ do
            printableStack (evalState funcEqual (Map.empty, Map.empty, [BOOL False, BOOL False])) `shouldBe` "[True]"
        it "printableStack (evalState funcEqual (Map.empty, Map.empty, [BOOL False, BOOL True])) returns \"[False]\"" $ do
            printableStack (evalState funcEqual (Map.empty, Map.empty, [BOOL False, BOOL True])) `shouldBe` "[False]"
        it "printableStack (evalState funcEqual (Map.empty, Map.empty, [INT 10, INT 10])) returns \"[True]\"" $ do
            printableStack (evalState funcEqual (Map.empty, Map.empty, [INT 10, INT 10])) `shouldBe` "[True]"
        it "printableStack (evalState funcEqual (Map.empty, Map.empty, [])) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcEqual (Map.empty, Map.empty, [])) `shouldBe` "[InvalidParameterAmount]"

spec_funcEmpty :: Spec
spec_funcEmpty = do
    describe "funcEmpty tests:" $ do
        it "printableStack (evalState funcEmpty (Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, [LIST \"0\"])) returns \"[False]\"" $ do
            printableStack (evalState funcEmpty (Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, [LIST "0"])) `shouldBe` "[False]"
        it "printableStack (evalState funcEmpty (Map.fromList [(\"0\", [])], Map.empty, [LIST \"0\"])) returns \"[True]\"" $ do
            printableStack (evalState funcEmpty (Map.fromList [("0", [])], Map.empty, [LIST "0"])) `shouldBe` "[True]"
        it "printableStack (evalState funcEmpty (Map.empty, Map.empty, [INT 10])) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcEmpty (Map.empty, Map.empty, [INT 10])) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcEmpty (Map.empty, Map.empty, [])) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcEmpty (Map.empty, Map.empty, [])) `shouldBe` "[InvalidParameterAmount]"

spec_funcHead :: Spec
spec_funcHead = do
    describe "funcHead tests:" $ do
        it "printableStack (evalState funcHead (Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, [LIST \"0\"])) returns \"[1]\"" $ do
            printableStack (evalState funcHead (Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, [LIST "0"])) `shouldBe` "[1]"
        it "evalState funcHead (Map.empty, Map.empty, [INT 10]) returns (Map.empty, [ERROR ExpectedList])" $ do
            evalState funcHead (Map.empty, Map.empty, [INT 10]) `shouldBe` (Map.empty, [ERROR ExpectedList])
        it "evalState funcHead (Map.empty, Map.empty, []) returns (Map.empty, [ERROR InvalidParameterAmount])" $ do
            evalState funcHead (Map.empty, Map.empty, []) `shouldBe` (Map.empty, [ERROR InvalidParameterAmount])

spec_funcTail :: Spec
spec_funcTail = do
    describe "funcTail tests:" $ do
        it "evalState funcTail (Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, [LIST \"0\"]) returns (Map.fromList [(\"0\", [INT 2, INT 3])], [LIST \"0\"])" $ do
            evalState funcTail (Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, [LIST "0"]) `shouldBe` (Map.fromList [("0", [INT 2, INT 3])], [LIST "0"])
        it "evalState funcTail (Map.empty, Map.empty, [INT 10]) returns (Map.empty, [ERROR ExpectedList])" $ do
            evalState funcTail (Map.empty, Map.empty, [INT 10]) `shouldBe` (Map.empty, [ERROR ExpectedList])
        it "evalState funcTail (Map.empty, Map.empty, []) returns (Map.empty, [ERROR InvalidParameterAmount])" $ do
            evalState funcTail (Map.empty, Map.empty, []) `shouldBe` (Map.empty, [ERROR InvalidParameterAmount])

spec_funcCons :: Spec
spec_funcCons = do
    describe "funcCons tests:" $ do
        it "printableStack (evalState funcCons (Map.fromList [(\"0\", [INT 2, INT 3])], Map.empty, [LIST \"0\", INT 1])) returns \"[[1, 2, 3]]\"" $ do
            printableStack (evalState funcCons (Map.fromList [("0", [INT 2, INT 3])], Map.empty, [LIST "0", INT 1])) `shouldBe` "[[1, 2, 3]]"
        it "printableStack (evalState funcCons (Map.fromList [(\"0\", []), (\"1\", [])], Map.empty, [LIST \"1\", LIST \"0\"])) returns \"[[[]]]\"" $ do
            printableStack (evalState funcCons (Map.fromList [("0", []), ("1", [])], Map.empty, [LIST "1", LIST "0"])) `shouldBe` "[[[]]]"
        it "printableStack (evalState funcCons (Map.empty, Map.empty, [INT 10, INT 10])) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcCons (Map.empty, Map.empty, [INT 10, INT 10])) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcCons (Map.empty, Map.empty, [])) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcCons (Map.empty, Map.empty, [])) `shouldBe` "[InvalidParameterAmount]"

spec_funcAppend :: Spec
spec_funcAppend = do
    describe "funcAppend tests:" $ do
        it "printableStack (evalState funcAppend (Map.fromList [(\"0\", [INT 2, INT 3]), (\"1\", [INT 1])], Map.empty, [LIST \"0\", LIST \"1\"])) returns \"[[1, 2, 3]]\"" $ do
            printableStack (evalState funcAppend (Map.fromList [("0", [INT 2, INT 3]), ("1", [INT 1])], Map.empty, [LIST "0", LIST "1"])) `shouldBe` "[[1, 2, 3]]"
        it "printableStack (evalState funcAppend (Map.fromList [(\"0\", []), (\"1\", [])], Map.empty, [LIST \"1\", LIST \"0\"])) returns \"[[]]\"" $ do
            printableStack (evalState funcAppend (Map.fromList [("0", []), ("1", [])], Map.empty, [LIST "1", LIST "0"])) `shouldBe` "[[]]"
        it "printableStack (evalState funcAppend (Map.empty, Map.empty, [INT 10, INT 10])) returns \"[ExpectedList]\"" $ do
            printableStack (evalState funcAppend (Map.empty, Map.empty, [INT 10, INT 10])) `shouldBe` "[ExpectedList]"
        it "printableStack (evalState funcAppend (Map.empty, Map.empty, [])) returns \"[InvalidParameterAmount]\"" $ do
            printableStack (evalState funcAppend (Map.empty, Map.empty, [])) `shouldBe` "[InvalidParameterAmount]"

-- module Parsing

spec_parseInput :: Spec
spec_parseInput = do
    describe "parseInput tests:" $ do
        it "evalState (parseInput \"1 2 +\") (Map.empty, Map.empty, []) returns (Map.empty, [INT 1, INT 2, FUNC \"+\"])" $ do
            evalState (parseInput "1 2 +") (Map.empty, Map.empty, []) `shouldBe` (Map.empty, [INT 1, INT 2, FUNC "+"])

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
        it "allocateObject [INT 1] (Map.fromList [(\"0\", []), (\"2\", [])]) returns Map.fromList [(\"0\", []), (\"2\", []), (\"1\", [INT 1])]" $ do
            allocateObject [INT 1] (Map.fromList [("0", []), ("2", [])]) `shouldBe` Map.fromList [("0", []), ("2", []), ("1", [INT 1])]

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
        it "printableStack (Map.empty, [INT 2, STRING \"a string\", INT 1]) returns \"[2, \"a string\", 1\"]" $ do
            printableStack (Map.empty, [INT 2, STRING "a string", INT 1]) `shouldBe` "[2, \"a string\", 1]"

spec_formatStack :: Spec
spec_formatStack = do
    describe "formatStack tests:" $ do
        it "formatStack [INT 2, STRING \"a string\", INT 1] \", \" Map.empty returns \"2, \"a string\", 1\"" $ do
            formatStack [INT 2, STRING "a string", INT 1] ", " Map.empty `shouldBe` "2, \"a string\", 1"
