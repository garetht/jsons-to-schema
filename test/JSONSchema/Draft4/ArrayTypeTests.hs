{-# LANGUAGE QuasiQuotes #-}

module JSONSchema.Draft4.ArrayTypeTests where

import           JSONSchema.Draft4.SchemaGenerationConfig
import           NeatInterpolation
import           Protolude
import           Test.Hspec
import           TestUtils

tupleTypedArrayConfig =
  defaultSchemaGenerationConfig {typeArraysAsTuples = True}

testSingleNonTupleArrayEmpty :: Spec
testSingleNonTupleArrayEmpty =
  it "can generate the schema for a single non-tuple typed array that is empty" $
  let j1 = [text| [] |]
      expected =
        [text|
            {"type": "array", "items": {}}
        |]
  in testJsonsToSchema [j1] expected

testSingleNonTupleArrayMonotype :: Spec
testSingleNonTupleArrayMonotype =
  it "can generate the schema for a single non-tuple typed array of one type" $
  let j1 = [text| ["spam", "spam", "spam", "eggs", "spam"] |]
      expected =
        [text|
            {"type": "array", "items": {"type": "string"}}
        |]
  in testJsonsToSchema [j1] expected

testSingleNonTupleArrayMultitype :: Spec
testSingleNonTupleArrayMultitype =
  it
    "can generate the schema for a single non-tuple typed array of multiple different types" $
  let j1 = [text| [1, "2", null, false] |]
      expected =
        [text|
          {
              "type": "array",
              "items": {
                  "type": ["boolean", "integer", "null", "string"]
              }
          }
        |]
  in testJsonsToSchema [j1] expected

testSingleNonTupleArrayNested :: Spec
testSingleNonTupleArrayNested =
  it
    "can generate the schema for a single non-tuple typed array with nested arrays" $
  let j1 =
        [text| [
                        ["surprise"],
                        ["fear", "surprise"],
                        ["fear", "surprise", "ruthless efficiency"],
                        ["fear", "surprise", "ruthless efficiency",
                         "an almost fanatical devotion to the Pope"]
                    ]
              |]
      expected =
        [text|
          {
              "type": "array",
              "items": {
                  "type": "array",
                  "items": {"type": "string"}}
          }
        |]
  in testJsonsToSchema [j1] expected

testSingleTupleArrayEmpty :: Spec
testSingleTupleArrayEmpty =
  it
    "can generate the schema for a single positionally typed tuple array that is empty" $
  let j1 = [text| [] |]
      expected =
        [text|
            {"type": "array"}
        |]
  in testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1] expected

testSingleTupleArrayMultitype :: Spec
testSingleTupleArrayMultitype =
  it
    "can generate the schema for a single positionally typed tuple array with different types at different positions" $ do
    let j1 = [text| [1, "2", "3", null, false] |]
    let expected =
          [text|
            {
                "type": "array",
                "items": [
                    {"type": "integer"},
                    {"type": "string"},
                    {"type": "string"},
                    {"type": "null"},
                    {"type": "boolean"}]
            }
        |]
    let invalid1 = [text| [1, 2, "3", null, false] |]
    testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1] expected
    expected `shouldNotValidateTexts` [invalid1]

testSingleTupleArrayNested :: Spec
testSingleTupleArrayNested =
  it
    "can generate the schema for a single positionally typed tuple array that is quite nested" $
  let j1 =
        [text| [
                        ["surprise"],
                        ["fear", "surprise"],
                        ["fear", "surprise", "ruthless efficiency"],
                        ["fear", "surprise", "ruthless efficiency",
                         "an almost fanatical devotion to the Pope"]
                    ] |]
      expected =
        [text|
        {
            "type": "array",
            "items": [
                {
                    "type": "array",
                    "items": [
                        {"type": "string"}
                    ]
                },
                {
                    "type": "array",
                    "items": [
                        {"type": "string"},
                        {"type": "string"}
                    ]
                },
                {
                    "type": "array",
                    "items": [
                        {"type": "string"},
                        {"type": "string"},
                        {"type": "string"}
                    ]
                },
                {
                    "type": "array",
                    "items": [
                        {"type": "string"},
                        {"type": "string"},
                        {"type": "string"},
                        {"type": "string"}
                    ]
                }
            ]
        }
        |]
  in testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1] expected

testNonTupleArrayEmpty :: Spec
testNonTupleArrayEmpty =
  it "can generate the schema for multiple non-tuple typed arrays" $
  let j1 = [text| [] |]
      j2 = [text| [] |]
      expected =
        [text|
            {"type": "array", "items": {}}
        |]
  in testJsonsToSchema [j1, j2] expected

testNonTupleArrayMonotype :: Spec
testNonTupleArrayMonotype =
  it
    "can generate the schema for multiple non-tuple typed arrays with only one type" $
  let j1 = [text| ["spam", "spam", "spam", "eggs", "spam"] |]
      j2 = [text| ["spam", "bacon", "eggs", "spam"] |]
      expected =
        [text|
            {"type": "array", "items": {"type": "string"}}
        |]
  in testJsonsToSchema [j1, j2] expected

testNonTupleArrayMultitype :: Spec
testNonTupleArrayMultitype =
  it
    "can generate the schema for multiple non-tuple typed arrays with multiple types" $
  let j1 = [text| [1, "2", "3", null, false] |]
      j2 = [text| [1, 2, "3", false] |]
      expected =
        [text|
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
  in testJsonsToSchema [j1, j2] expected

testNonTupleArrayNested :: Spec
testNonTupleArrayNested =
  it
    "can generate the schema for multiple non-tuple typed arrays with nested array types" $
  let j1 =
        [text|
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
      j2 =
        [text|
        [
            ["fear", "surprise", "ruthless efficiency"],
            ["fear", "surprise", "ruthless efficiency",
             "an almost fanatical devotion to the Pope"]
        ]
        |]
      expected =
        [text|
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
  in testJsonsToSchema [j1, j2] expected

testTupleArraysEmpty :: Spec
testTupleArraysEmpty =
  it "can generate the schema for multiple tuple typed arrays that are empty" $
  let j1 = [text| [] |]
      j2 = [text| [] |]
      expected =
        [text|
            {"type": "array"}
        |]
  in testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1, j2] expected

testTupleArraysMultitype :: Spec
testTupleArraysMultitype =
  it
    "can generate the schema for multiple tuple typed arrays that have different types in each position" $ do
    let j1 = [text| [1, "2", "3", null, false] |]
    let j2 = [text| [1, 2, "3", false] |]
    let expected =
          [text|
              {
                "type": "array",
                "items": [
                    {"type": "integer"},
                    {"type": ["integer", "string"]},
                    {"type": "string"},
                    {"type": ["boolean", "null"]},
                    {"type": "boolean"}]
              }
          |]
    let invalid1 = [text| [1, 2, 3, null, false] |]
    testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1, j2] expected
    expected `shouldNotValidateTexts` [invalid1]

testTupleArraysNested :: Spec
testTupleArraysNested =
  it "can generate the schema for multiple tuple typed arrays that are nested" $
  let j1 =
        [text| [
                        ["surprise"],
                        ["fear", "surprise"]
                    ] |]
      j2 =
        [text| [
                        ["fear", "surprise", "ruthless efficiency"],
                        ["fear", "surprise", "ruthless efficiency",
                         "an almost fanatical devotion to the Pope"]
                    ] |]
      expected =
        [text|
        {
            "type": "array",
            "items": [
                {
                    "type": "array",
                    "items": [
                        {"type": "string"},
                        {"type": "string"},
                        {"type": "string"}
                    ]
                },
                {
                    "type": "array",
                    "items": [
                        {"type": "string"},
                        {"type": "string"},
                        {"type": "string"},
                        {"type": "string"}
                    ]
                }
            ]
        }
        |]
  in testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1, j2] expected
