{-# LANGUAGE QuasiQuotes #-}

module JSONSchema.SchemaConverterSpec where

import           NeatInterpolation
import           Protolude
import           Test.Hspec
import           TestUtils


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


testSinglyTypedArrayEmpty :: Spec
testSinglyTypedArrayEmpty = it "can generate the schema for a singly-typed (non-tuple typed) array" $
    let j1 = [text| [] |]
        j2 = [text| [] |]
        expected = [text|
            {"type": "array", "items": {}}
        |]
    in
        testJsonsToSchema [j1, j2] expected

testSinglyTypedArrayMonotype :: Spec
testSinglyTypedArrayMonotype = it "can generate the schema for a singly-typed (non-tuple typed) array with only one type" $
    let j1 = [text| ["spam", "spam", "spam", "eggs", "spam"] |]
        j2 = [text| ["spam", "bacon", "eggs", "spam"] |]
        expected = [text|
            {"type": "array", "items": {"type": "string"}}
        |]
    in
        testJsonsToSchema [j1, j2] expected

testSinglyTypedArrayMultitype :: Spec
testSinglyTypedArrayMultitype = it "can generate the schema for a singly-typed (non-tuple typed) array with multiple types" $
    let j1 = [text| [1, "2", "3", null, false] |]
        j2 = [text| [1, 2, "3", false] |]
        expected = [text|
            {
              "type":"array",
              "items":{
                "type":[
                  "boolean",
                  "integer",
                  "null",
                  "string"
                ]
              }
            }
        |]
    in
        testJsonsToSchema [j1, j2] expected

testSinglyTypedArrayNested :: Spec
testSinglyTypedArrayNested = it "can generate the schema for a singly-typed (non-tuple typed) array with nested array types" $
    let j1 = [text|
        [
          [
            "surprise"
          ],
          [
            "fear",
            "surprise"
          ]
        ]
        |]
        j2 = [text|
        [
            ["fear", "surprise", "ruthless efficiency"],
            ["fear", "surprise", "ruthless efficiency",
             "an almost fanatical devotion to the Pope"]
        ]
        |]
        expected = [text|
        {
          "type":"array",
          "items":{
            "type":"array",
            "items":{
              "type":"string"
            }
          }
        }
        |]
    in
        testJsonsToSchema [j1, j2] expected



spec :: Spec
spec = do
    describe "Basic Types" $ do
        testBasicTypesSingleType
        testBasicTypesMultipleType
        testBasicTypesIntegerType

    describe "Array Type" $ do
         testSinglyTypedArrayEmpty
         testSinglyTypedArrayMonotype
         testSinglyTypedArrayMultitype
         testSinglyTypedArrayNested
