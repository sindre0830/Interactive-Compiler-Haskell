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
        -- module Parsing
        spec_tokenize
        spec_parser
        spec_listParser
        spec_stringParser
        spec_typeParser

-- module Parsing

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

spec_listParser :: Spec
spec_listParser = do
    describe "listParser tests:" $ do
        it "listParser (tokenize \"1 2 ]\") [] Map.empty returns ([INT 2, INT 1], [], Map.empty)" $ do
            listParser (tokenize "1 2 ]") [] Map.empty `shouldBe` ([INT 2, INT 1], [], Map.empty)
        it "listParser (tokenize \"[ ] ]\") [] Map.empty returns ([LIST \"0\"], [], (Map.fromList [(\"0\", [])]))" $ do
            listParser (tokenize "[ ] ]") [] Map.empty `shouldBe` ([LIST "0"], [], (Map.fromList [("0", [])]))
        it "listParser (tokenize \"1 [ 2 ] 3 ]\") [] Map.empty returns ([INT 3, LIST \"0\", INT 1], [], (Map.fromList [(\"0\", [INT 2])]))" $ do
            listParser (tokenize "1 [ 2 ] 3 ]") [] Map.empty `shouldBe` ([INT 3, LIST "0", INT 1], [], (Map.fromList [("0", [INT 2])]))
        it "listParser (tokenize \"[ [ ] ] [ ] ]\") [] Map.empty returns ([LIST \"2\", LIST \"0\"], [], (Map.fromList [(\"0\", [LIST \"1\"]), (\"1\", []), (\"2\", [])]))" $ do
            listParser (tokenize "[ [ ] ] [ ] ]") [] Map.empty `shouldBe` ([LIST "2", LIST "0"], [], (Map.fromList [("0", [LIST "1"]), ("1", []), ("2", [])]))
        it "listParser (tokenize \"1 \" a string \" 2 ]\") [] Map.empty returns ([INT 2, STRING \"a string\", INT 1], [], Map.empty)" $ do
            listParser (tokenize "1 \" a string \" 2 ]") [] Map.empty `shouldBe` ([INT 2, STRING "a string", INT 1], [], Map.empty)

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
