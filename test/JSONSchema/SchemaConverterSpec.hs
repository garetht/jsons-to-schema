{-# LANGUAGE QuasiQuotes #-}

module JSONSchema.SchemaConverterSpec where

import           JSONSchema.SchemaGenerationConfig
import           NeatInterpolation
import           Protolude
import           Test.Hspec
import           TestUtils


tupleTypedArrayConfig = defaultSchemaGenerationConfig {
  typeArraysAsTuples = True
}

sealedObjectPropertiesConfig = defaultSchemaGenerationConfig {
  sealObjectProperties = True
}

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
    let j1 = [text| [1, "2", null, false] |]
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

testSingleTupleArrayEmpty :: Spec
testSingleTupleArrayEmpty = it "can generate the schema for a single positionally typed tuple array that is empty" $
    let j1 = [text| [] |]
        expected = [text|
            {"type": "array"}
        |]
    in
        testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1] expected

testSingleTupleArrayMultitype :: Spec
testSingleTupleArrayMultitype = it "can generate the schema for a single positionally typed tuple array with different types at different positions" $ do
    let j1 = [text| [1, "2", "3", null, false] |]
    let expected = [text|
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
    expected `shouldNotValidate` [invalid1]

testSingleTupleArrayNested :: Spec
testSingleTupleArrayNested = it "can generate the schema for a single positionally typed tuple array that is quite nested" $
    let j1 = [text| [
                        ["surprise"],
                        ["fear", "surprise"],
                        ["fear", "surprise", "ruthless efficiency"],
                        ["fear", "surprise", "ruthless efficiency",
                         "an almost fanatical devotion to the Pope"]
                    ] |]
        expected = [text|
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
    in
        testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1] expected

testSingleEmptyObject :: Spec
testSingleEmptyObject = it "can generate the schema for a single empty object" $
    let j1 = [text| {} |]
        expected = [text| {"type": "object", "properties": {}} |]
    in
        testJsonsToSchema [j1] expected

testSingleBasicObject :: Spec
testSingleBasicObject = it "can generate the schema for a single basic object" $
    let j1 = [text| {
                      "Red Windsor": "Normally, but today the van broke down.",
                      "Stilton": "Sorry.",
                      "Gruyere": false
                    } |]
        expected = [text| {
                            "required": ["Gruyere", "Red Windsor", "Stilton"],
                            "type": "object",
                            "properties": {
                                "Red Windsor": {"type": "string"},
                                "Gruyere": {"type": "boolean"},
                                "Stilton": {"type": "string"}
                            }
                          } |]
    in
        testJsonsToSchema [j1] expected

testSingleBasicObjectSealingProperties :: Spec
testSingleBasicObjectSealingProperties = it "can generate the schema for a single basic object while sealing properties" $
    let j1 = [text| {
                      "Red Windsor": "Normally, but today the van broke down.",
                      "Stilton": "Sorry.",
                      "Gruyere": false
                    } |]
        expected = [text| {
                            "required": ["Gruyere", "Red Windsor", "Stilton"],
                            "type": "object",
                            "properties": {
                                "Red Windsor": {"type": "string"},
                                "Gruyere": {"type": "boolean"},
                                "Stilton": {"type": "string"}
                            },
                            "additionalProperties": false
                          } |]
    in
        testJsonsToSchemaWithConfig sealedObjectPropertiesConfig [j1] expected

testComplexArrayInObject :: Spec
testComplexArrayInObject = it "can generate the schema for arrays that are in objects" $
    let j1 = [text| {"a": "b", "c": [1, 2, 3]} |]
        expected = [text| {
                            "required": ["a", "c"],
                            "type": "object",
                            "properties": {
                                "a": {"type": "string"},
                                "c": {
                                    "type": "array",
                                    "items": {"type": "integer"}
                                }
                            }
                          }
                    |]
    in
        testJsonsToSchema [j1] expected

testComplexObjectInArray :: Spec
testComplexObjectInArray = it "can generate the schema for objects that are in arrays" $
    let j1 = [text| [
                      {"name": "Sir Lancelot of Camelot",
                       "quest": "to seek the Holy Grail",
                       "favorite colour": "blue"},
                      {"name": "Sir Robin of Camelot",
                       "quest": "to seek the Holy Grail",
                       "capitol of Assyria": null
                       }]
             |]
        expected = [text| {
                              "type": "array",
                              "items": {
                                  "type": "object",
                                  "required": ["name", "quest"],
                                  "properties": {
                                      "quest": {"type": "string"},
                                      "name": {"type": "string"},
                                      "favorite colour": {"type": "string"},
                                      "capitol of Assyria": {"type": "null"}
                                  }
                              }
                          }
                    |]
    in
        testJsonsToSchema [j1] expected

testComplexObjectInArraySealingProperties :: Spec
testComplexObjectInArraySealingProperties = it "can generate the schema for objects that are in arrays while sealing properties" $
    let j1 = [text| [
                      {"name": "Sir Lancelot of Camelot",
                       "quest": "to seek the Holy Grail",
                       "favorite colour": "blue"},
                      {"name": "Sir Robin of Camelot",
                       "quest": "to seek the Holy Grail",
                       "capitol of Assyria": null
                       }]
             |]
        expected = [text| {
                              "type": "array",
                              "items": {
                                  "type": "object",
                                  "required": ["name", "quest"],
                                  "properties": {
                                      "quest": {"type": "string"},
                                      "name": {"type": "string"},
                                      "favorite colour": {"type": "string"},
                                      "capitol of Assyria": {"type": "null"}
                                  },
                                  "additionalProperties": false
                              }
                          }
                    |]
    in
        testJsonsToSchemaWithConfig sealedObjectPropertiesConfig [j1] expected

testComplexThreeDeepObject :: Spec
testComplexThreeDeepObject = it "can generate the schema for a deeply nested object" $
    let j1 = [text| {"matryoshka": {"design": {"principle": "FTW!"}}} |]
        expected = [text| {
                            "type": "object",
                            "required": ["matryoshka"],
                            "properties": {
                                "matryoshka": {
                                    "type": "object",
                                    "required": ["design"],
                                    "properties": {
                                        "design": {
                                            "type": "object",
                                            "required": ["principle"],
                                            "properties": {
                                                "principle": {"type": "string"}
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    |]
    in
        testJsonsToSchema [j1] expected

testComplexThreeDeepObjectSealingProperties :: Spec
testComplexThreeDeepObjectSealingProperties = it "can generate the schema for a deeply nested object while sealing properties" $
    let j1 = [text| {"matryoshka": {"design": {"principle": "FTW!"}}} |]
        expected = [text| {
                            "type": "object",
                            "required": ["matryoshka"],
                            "properties": {
                                "matryoshka": {
                                    "type": "object",
                                    "required": ["design"],
                                    "properties": {
                                        "design": {
                                            "type": "object",
                                            "required": ["principle"],
                                            "properties": {
                                                "principle": {"type": "string"}
                                            },
                                            "additionalProperties": false
                                        }
                                    },
                                    "additionalProperties": false
                                }
                            },
                            "additionalProperties": false
                        }
                    |]
    in
        testJsonsToSchemaWithConfig sealedObjectPropertiesConfig [j1] expected

testMultipleEmptyObjects :: Spec
testMultipleEmptyObjects = it "can generate the schema for multiple empty objects" $
    let j1 = [text| {} |]
        j2 = [text| {} |]
        expected = [text| {"type": "object", "properties": {}} |]
    in
        testJsonsToSchema [j1, j2] expected

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

testTupleArraysEmpty :: Spec
testTupleArraysEmpty = it "can generate the schema for multiple tuple typed arrays that are empty" $
    let j1 = [text| [] |]
        j2 = [text| [] |]
        expected = [text|
            {"type": "array"}
        |]
    in
        testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1, j2] expected

testTupleArraysMultitype :: Spec
testTupleArraysMultitype = it "can generate the schema for multiple tuple typed arrays that have different types in each position" $ do
      let j1 = [text| [1, "2", "3", null, false] |]
      let j2 = [text| [1, 2, "3", false] |]
      let expected = [text|
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
      expected `shouldNotValidate` [invalid1]

testTupleArraysNested :: Spec
testTupleArraysNested = it "can generate the schema for multiple tuple typed arrays that are nested" $
    let j1 = [text| [
                        ["surprise"],
                        ["fear", "surprise"]
                    ] |]
        j2 = [text| [
                        ["fear", "surprise", "ruthless efficiency"],
                        ["fear", "surprise", "ruthless efficiency",
                         "an almost fanatical devotion to the Pope"]
                    ] |]
        expected = [text|
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
    in
        testJsonsToSchemaWithConfig tupleTypedArrayConfig [j1, j2] expected

spec :: Spec
spec = do
    describe "Single Instances of Basic Types" $ do
      testBasicTypesSingleStringInstance
      testBasicTypesSingleIntegerInstance
      testBasicTypesSingleNumberInstance
      testBasicTypesSingleBooleanInstance
      testBasicTypesSingleNullInstance

    describe "Single Instances of the Non-Tuple Array Type" $ do
      testSingleNonTupleArrayEmpty
      testSingleNonTupleArrayMonotype
      testSingleNonTupleArrayMultitype
      testSingleNonTupleArrayNested

    describe "Single Instances of the Tuple Array Type" $ do
      testSingleTupleArrayEmpty
      testSingleTupleArrayMultitype
      testSingleTupleArrayNested

    describe "Single Instances of Object Types" $ do
      testSingleEmptyObject
      testSingleBasicObject
      testSingleBasicObjectSealingProperties

    describe "Single Instances of More Complex Types" $ do
      testComplexArrayInObject
      testComplexObjectInArray
      testComplexObjectInArraySealingProperties
      testComplexThreeDeepObject
      testComplexThreeDeepObjectSealingProperties

    describe "Combining Multiple Instances of Basic Types" $ do
      testBasicTypesSingleType
      testBasicTypesMultipleType
      testBasicTypesIntegerType

    describe "Combining Multiple Instances of the Non-Tuple Array Type" $ do
      testNonTupleArrayEmpty
      testNonTupleArrayMonotype
      testNonTupleArrayMultitype
      testNonTupleArrayNested

    describe "Combining Multiple Instances of the Tuple Array Type" $ do
      testTupleArraysEmpty
      testTupleArraysMultitype
      testTupleArraysNested

    describe "Combining Multiple Instances of Object Types"
      testMultipleEmptyObjects
