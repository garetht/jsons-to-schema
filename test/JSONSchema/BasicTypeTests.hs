{-# LANGUAGE QuasiQuotes #-}

module JSONSchema.BasicTypeTests where

import           JSONSchema.SchemaGenerationConfig
import           NeatInterpolation
import           Protolude
import           Test.Hspec
import           TestUtils


testBasicTypesSingleStringInstance :: Spec
testBasicTypesSingleStringInstance = it "can handle a single instance of a string" $
    let j1 = [text| "string" |]
        expected = [text|
            {"type": "string"}
        |]
    in
        testJsonsToSchema [j1] expected

testBasicTypesSingleIntegerInstance :: Spec
testBasicTypesSingleIntegerInstance = it "can handle a single instance of a integer" $
    let j1 = [text| 1 |]
        expected = [text|
            {"type": "integer"}
        |]
    in
        testJsonsToSchema [j1] expected

testBasicTypesSingleNumberInstance :: Spec
testBasicTypesSingleNumberInstance = it "can handle a single instance of a number" $
    let j1 = [text| 2.1 |]
        expected = [text|
            {"type": "number"}
        |]
    in
        testJsonsToSchema [j1] expected

testBasicTypesSingleBooleanInstance :: Spec
testBasicTypesSingleBooleanInstance = it "can handle a single instance of a boolean" $
    let j1 = [text| true |]
        expected = [text|
            {"type": "boolean"}
        |]
    in
        testJsonsToSchema [j1] expected

testBasicTypesSingleNullInstance :: Spec
testBasicTypesSingleNullInstance = it "can handle a single instance of a null" $
    let j1 = [text| null |]
        expected = [text|
            {"type": "null"}
        |]
    in
        testJsonsToSchema [j1] expected


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
        j2 = [text| 2.2 |]
        j3 = [text| true |]
        j4 = [text| null |]
        expected = [text|
            {"type": ["boolean", "null", "number", "string"]}
        |]
    in
        testJsonsToSchema [j1, j2, j3, j4] expected

testBasicTypesIntegerType :: Spec
testBasicTypesIntegerType = it "can handle the Integer type when given an integer" $
    -- TODO: this should eventually unify to just number
    let j1 = [text| 2.14 |]
        j2 = [text| 32 |]
        expected = [text|
            {"type": ["number", "integer"]}
        |]
    in
        testJsonsToSchema [j1, j2] expected
