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
        spec_funcHead
        spec_funcTail
        -- module Parsing
        spec_parseInput
        spec_tokenize
        spec_parser
        spec_codeBlockParser
        spec_listParser
        spec_stringParser
        spec_typeParser
        -- module Stack
        spec_printableStack
        spec_formatStack

-- module compiler

spec_funcHead :: Spec
spec_funcHead = do
    describe "funcHead tests:" $ do
        it "evalState funcHead (Map.fromList [(\"0\", [INT 1, INT 2, INT 3])], Map.empty, [LIST \"0\"]) returns (Map.empty, [INT 1])" $ do
            evalState funcHead (Map.fromList [("0", [INT 1, INT 2, INT 3])], Map.empty, [LIST "0"]) `shouldBe` (Map.empty, [INT 1])
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
-- module Parsing

spec_parseInput :: Spec
spec_parseInput = do
    describe "parseInput tests:" $ do
        it "evalState (parseInput \"1 2 +\") (Map.empty, Map.empty, []) returns (Map.empty, [FUNC \"+\", INT 2, INT 1])" $ do
            evalState (parseInput "1 2 +") (Map.empty, Map.empty, []) `shouldBe` (Map.empty, [FUNC "+", INT 2, INT 1])

spec_tokenize :: Spec
spec_tokenize = do
    describe "tokenize tests:" $ do
        it "tokenize \"1 2 +\" returns [\"1\", \"2\", \"+\"]" $ do
            tokenize "1 2 +" `shouldBe` ["1", "2", "+"]

spec_parser :: Spec
spec_parser = do
    describe "parser tests:" $ do
        it "parser [\"1\", \"2\", \"+\"] [] Map.empty returns ([FUNC \"+\", INT 2, INT 1], Map.empty)" $ do
            parser ["1", "2", "+"] [] Map.empty `shouldBe` ([FUNC "+", INT 2, INT 1], Map.empty)
        it "parser [\"1\", \", \"a\", \"string\", \"] [] Map.empty returns ([STRING \"a string\", INT 1], Map.empty)" $ do
            parser ["1", "\"", "a", "string", "\""] [] Map.empty `shouldBe` ([STRING "a string", INT 1], Map.empty)
        it "parser [\", \"a\", \"string\", \", \"1\"] [] Map.empty returns ([INT 1, STRING \"a string\"], Map.empty)" $ do
            parser ["\"", "a", "string", "\"", "1"] [] Map.empty `shouldBe` ([INT 1, STRING "a string"], Map.empty)
        it "parser [\", \"a\", \"string\", \"] [] Map.empty returns ([STRING \"a string\"], Map.empty)" $ do
            parser ["\"", "a", "string", "\""] [] Map.empty `shouldBe` ([STRING "a string"], Map.empty)
        it "parser [\", \"a\", \"string\", \", \", \"b\", \"string\", \"] [] Map.empty returns ([STRING \"b string\", STRING \"a string\"], Map.empty)" $ do
            parser ["\"", "a", "string", "\"", "\"", "b", "string", "\""] [] Map.empty `shouldBe` ([STRING "b string", STRING "a string"], Map.empty)
        it "parser (tokenize \"1 2 + { 10 * [ { 1 2 + } 2 ] }\") [] Map.empty returns \
                \  ([CODEBLOCK \"0\", FUNC \"+\", INT 2, INT 1], Map.fromList [(\"0\", [LIST \"1\", FUNC \"*\", INT 10]), (\"1\", [INT 2, CODEBLOCK \"2\"]), (\"2\", [FUNC \"+\", INT 2, INT 1])])" $ do
            parser (tokenize "1 2 + { 10 * [ { 1 2 + } 2 ] }") [] Map.empty `shouldBe` ([CODEBLOCK "0", FUNC "+", INT 2, INT 1], Map.fromList [("0", [LIST "1", FUNC "*", INT 10]), ("1", [INT 2, CODEBLOCK "2"]), ("2", [FUNC "+", INT 2, INT 1])])
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
        it "codeBlockParser (tokenize \"{ { } } { } }\") [] Map.empty returns ([CODEBLOCK \"2\", CODEBLOCK \"0\"], [], (Map.fromList [(\"0\", [CODEBLOCK \"1\"]), (\"1\", []), (\"2\", [])]))" $ do
            codeBlockParser (tokenize "{ { } } { } }") [] Map.empty `shouldBe` ([CODEBLOCK "2", CODEBLOCK "0"], [], Map.fromList [("0", [CODEBLOCK "1"]), ("1", []), ("2", [])])
        it "codeBlockParser (tokenize \"1 \" a string \" 2 }\") [] Map.empty returns ([INT 2, STRING \"a string\", INT 1], [], Map.empty)" $ do
            codeBlockParser (tokenize "1 \" a string \" 2 }") [] Map.empty `shouldBe` ([INT 2, STRING "a string", INT 1], [], Map.empty)
        it "codeBlockParser (tokenize \"1 2 [] }\") [] Map.empty returns ({LIST 0, INT 2, INT 1},[], Map.empty)" $ do
            codeBlockParser (tokenize "1 2 [ ] }") [] Map.empty `shouldBe` ([LIST "0", INT 2, INT 1], [], Map.fromList [("0", [])])
        it "codeBlockParser (tokenize \"1 2 [ 4 { } ] }\") [] Map.empty returns ({LIST 0, INT 2, INT 1}, [], Map.empty)" $ do
            codeBlockParser (tokenize "1 2 [ 4 { } ] }") [] Map.empty `shouldBe` ([LIST "0", INT 2, INT 1], [], Map.fromList [("0", [CODEBLOCK "1", INT 4]), ("1", [])])

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
        it "listParser (tokenize \"[ [ ] ] [ ] ]\") [] Map.empty returns ([LIST \"2\", LIST \"0\"], [], (Map.fromList [(\"0\", [LIST \"1\"]), (\"1\", []), (\"2\", [])]))" $ do
            listParser (tokenize "[ [ ] ] [ ] ]") [] Map.empty `shouldBe` ([LIST "2", LIST "0"], [], Map.fromList [("0", [LIST "1"]), ("1", []), ("2", [])])
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

spec_printableStack :: Spec
spec_printableStack = do
    describe "printableStack tests:" $ do
        it "printableStack (Map.empty, [INT 2, STRING \"a string\", INT 1]) returns \"[1, \"a string\", 2\"]" $ do
            printableStack (Map.empty, [INT 2, STRING "a string", INT 1]) `shouldBe` "[1, \"a string\", 2]"

spec_formatStack :: Spec
spec_formatStack = do
    describe "formatStack tests:" $ do
        it "formatStack [INT 2, STRING \"a string\", INT 1] \", \" Map.empty returns \"1, \"a string\", 2\"" $ do
            formatStack [INT 2, STRING "a string", INT 1] ", " Map.empty `shouldBe` "1, \"a string\", 2"
