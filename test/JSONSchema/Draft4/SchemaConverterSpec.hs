module JSONSchema.Draft4.SchemaConverterSpec where

import           JSONSchema.Draft4
import           JSONSchema.Draft4.ArrayTypeTests
import           JSONSchema.Draft4.BasicTypeTests
import           JSONSchema.Draft4.ComplexTypeTests
import           JSONSchema.Draft4.QuickCheckTests
import           JSONSchema.Draft4.SchemaGenerationConfig
import           NeatInterpolation
import           Protolude
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils

-- Many of these tests are borrowed from Python's GenSON
-- https://github.com/wolverdude/GenSON

spec :: Spec
spec = do
  describe "Single Instances of Basic Types" $ do
    testBasicTypesSingleStringInstance
    testBasicTypesSingleIntegerInstance
    testBasicTypesSingleNumberInstance
    testBasicTypesSingleBooleanInstance
    testBasicTypesSingleNullInstance
  describe "Combining Multiple Instances of Basic Types" $ do
    testBasicTypesSingleType
    testBasicTypesMultipleType
    testBasicTypesIntegerType
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
    testEdgeCaseNestedSchema
  describe "Combining Multiple Instances of the Non-Tuple Array Type" $ do
    testNonTupleArrayEmpty
    testNonTupleArrayMonotype
    testNonTupleArrayMultitype
    testNonTupleArrayNested
  describe "Combining Multiple Instances of the Tuple Array Type" $ do
    testTupleArraysEmpty
    testTupleArraysMultitype
    testTupleArraysNested
  describe
    "Combining Multiple Instances of Object Types"
    testMultipleEmptyObjects
  describe "Schema Properties" $ do
    testPropUnifyEmptySchemaRightIdentity
    testPropUnifyEmptySchemaLeftIdentity
    testJsonToSchemaWithConfigValidatesJson
    testSchemaUnificationValidatesAllJson
