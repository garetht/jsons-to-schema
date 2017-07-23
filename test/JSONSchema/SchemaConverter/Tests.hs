module JSONSchema.SchemaConverter.Tests where

import           Protolude
import           Test.Hspec
import           TestUtils
import NeatInterpolation


-- These tests are helpfully borrowed from Python's GenSON
-- https://github.com/wolverdude/GenSON

testBasicTypesSingleType :: Spec
testBasicTypesSingleType = it "can handle multiple JSON examples of a single type" $
    let j1 = [text| "bacon" |]
        j2 = [text| "eggs" |]
        j3 = [text| "spam" |]
        expected = [text|
            {"type": "string"}
        |]
    in
        testJsonsToSchema [j1, j2, j3] expected

testBasicTypesMultipleType :: Spec
testBasicTypesMultipleType = it "can handle multiple JSON examples of multiple different types" $
    let j1 = [text| "bacon" |]
        j2 = [text| 2 |]
        j3 = [text| true |]
        j4 = [text| null |]
        expected = [text|
            {"type": ["boolean", "null", "number", "string"]}
        |]
    in
        testJsonsToSchema [j1, j2, j3, j4] expected

simpleExternalTest :: Spec
simpleExternalTest = do
    describe "Basic Types" $ do
        testBasicTypesSingleType
        testBasicTypesMultipleType

    describe "Other Types" $ do
         testBasicTypesSingleType
         testBasicTypesMultipleType
