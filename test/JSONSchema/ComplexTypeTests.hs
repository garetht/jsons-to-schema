{-# LANGUAGE QuasiQuotes #-}

module JSONSchema.ComplexTypeTests where

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

testEdgeCaseNestedSchema :: Spec
testEdgeCaseNestedSchema = it "can generate the schema for strangely nested objects" $
    let j1 = [text| [
                        [
                            null,
                            [
                                null
                            ]
                        ],
                        {},
                        {
                            "!": false
                        }
                    ] |]
        expected = [text|
          {
            "items": {
                "items": {
                    "items": {
                        "type": "null"
                    },
                    "type": [
                        "array",
                        "null"
                    ]
                },
                "type": [
                    "object",
                    "array"
                ],
                "properties": {
                    "!": {
                        "type": "boolean"
                    }
                }
            },
            "type": "array"
          }
         |]
    in
        testJsonsToSchemaPretty [j1] expected
