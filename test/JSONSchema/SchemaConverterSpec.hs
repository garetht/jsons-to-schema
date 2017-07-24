{-# LANGUAGE QuasiQuotes #-}

module JSONSchema.SchemaConverterSpec where

import           NeatInterpolation
import           Protolude
import           Test.Hspec
import           TestUtils


-- These tests are helpfully borrowed from Python's GenSON
-- https://github.com/wolverdude/GenSON
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

testSingleNonTupleArrayEmpty :: Spec
testSingleNonTupleArrayEmpty = it "can generate the schema for a single non-tuple typed array that is empty" $
    let j1 = [text| [] |]
        expected = [text|
            {"type": "array", "items": {}}
        |]
    in
        testJsonsToSchema [j1] expected

testSingleNonTupleArrayMonotype :: Spec
testSingleNonTupleArrayMonotype = it "can generate the schema for a single non-tuple typed array of one type" $
    let j1 = [text| ["spam", "spam", "spam", "eggs", "spam"] |]
        expected = [text|
            {"type": "array", "items": {"type": "string"}}
        |]
    in
        testJsonsToSchema [j1] expected

testSingleNonTupleArrayMultitype :: Spec
testSingleNonTupleArrayMultitype = it "can generate the schema for a single non-tuple typed array of multiple different types" $
    let j1 = [text| [1, "2", None, False] |]
        expected = [text|
          {
              "type": "array",
              "items": {
                  "type": ["boolean", "integer", "null", "string"]
              }
          }
        |]
    in
        testJsonsToSchema [j1] expected

testSingleNonTupleArrayNested :: Spec
testSingleNonTupleArrayNested = it "can generate the schema for a single non-tuple typed array with nested arrays" $
    let j1 = [text| [
                        ["surprise"],
                        ["fear", "surprise"],
                        ["fear", "surprise", "ruthless efficiency"],
                        ["fear", "surprise", "ruthless efficiency",
                         "an almost fanatical devotion to the Pope"]
                    ]
              |]
        expected = [text|
          {
              "type": "array",
              "items": {
                  "type": "array",
                  "items": {"type": "string"}}
          }
        |]
    in
        testJsonsToSchema [j1] expected

testNonTupleArrayEmpty :: Spec
testNonTupleArrayEmpty = it "can generate the schema for multiple non-tuple typed arrays" $
    let j1 = [text| [] |]
        j2 = [text| [] |]
        expected = [text|
            {"type": "array", "items": {}}
        |]
    in
        testJsonsToSchema [j1, j2] expected

testNonTupleArrayMonotype :: Spec
testNonTupleArrayMonotype = it "can generate the schema for multiple non-tuple typed arrays with only one type" $
    let j1 = [text| ["spam", "spam", "spam", "eggs", "spam"] |]
        j2 = [text| ["spam", "bacon", "eggs", "spam"] |]
        expected = [text|
            {"type": "array", "items": {"type": "string"}}
        |]
    in
        testJsonsToSchema [j1, j2] expected

testNonTupleArrayMultitype :: Spec
testNonTupleArrayMultitype = it "can generate the schema for multiple non-tuple typed arrays with multiple types" $
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

testNonTupleArrayNested :: Spec
testNonTupleArrayNested = it "can generate the schema for multiple non-tuple typed arrays with nested array types" $
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
    describe "Single Instances of Basic Types" $ do
        testBasicTypesSingleStringInstance
        testBasicTypesSingleIntegerInstance
        testBasicTypesSingleNumberInstance
        testBasicTypesSingleBooleanInstance
        testBasicTypesSingleNullInstance

--     describe "Single Instances of the Non-Tuple Array Type"

    describe "Combining Multiple Instances of Basic Types" $ do
        testBasicTypesSingleType
        testBasicTypesMultipleType
        testBasicTypesIntegerType

    describe "Combining Multiple Instances of the Non-Tuple Array Type" $ do
         testNonTupleArrayEmpty
         testNonTupleArrayMonotype
         testNonTupleArrayMultitype
         testNonTupleArrayNested
