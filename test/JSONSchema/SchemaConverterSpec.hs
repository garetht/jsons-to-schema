module JSONSchema.SchemaConverterSpec where

import           JSONSchema.SchemaGenerationConfig
import           NeatInterpolation
import           Protolude
import           Test.Hspec
import           TestUtils
import           JSONSchema.BasicTypeTests
import           JSONSchema.ArrayTypeTests
import           JSONSchema.ComplexTypeTests
import           JSONSchema.QuickCheckTests
import           JSONSchema.Draft4
import Test.QuickCheck

import Prelude (read)

-- These tests are helpfully borrowed from Python's GenSON
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

  describe "Schema Properties" $ do
    testPropUnifyEmptySchemaRightIdentity
    testPropUnifyEmptySchemaLeftIdentity

