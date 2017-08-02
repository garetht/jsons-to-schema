{-|
  Module      : JSONSchema.Draft4.SchemaGenerationConfig
  Description : Configuration options for schema generation
  Copyright   : (c) Gareth Tan, 2017
  License     : MIT

  A single JSON document can be interpreted multiple ways and
  with differing degrees of strictness when it is converted
  into a JSON Schema. These options allow the way in which a
  JSON document is converted into a schema to be customized.
-}
module JSONSchema.Draft4.SchemaConfig
  ( SchemaConfig(..)
  , defaultSchemaConfig
  , SchemaGenerationConfig(..)
  , defaultSchemaGenerationConfig
  , SchemaUnificationConfig(..)
  , defaultSchemaUnificationConfig
  ) where

import           Protolude
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof, sized)

data SchemaConfig = SchemaConfig
  { generationConfig  :: SchemaGenerationConfig
  , unificationConfig :: SchemaUnificationConfig
  } deriving (Show)

defaultSchemaConfig :: SchemaConfig
defaultSchemaConfig =
  SchemaConfig {generationConfig = defaultSchemaGenerationConfig, unificationConfig = defaultSchemaUnificationConfig}

instance Arbitrary SchemaConfig where
  arbitrary = sized f
    where
      f :: Int -> Gen SchemaConfig
      f n = do
        c1 <- arbitrary
        c2 <- arbitrary
        pure $ SchemaConfig c1 c2

{-| Allows options to be specified for turning an individual JSON document
    into a JSON schema.
-}
data SchemaGenerationConfig = SchemaGenerationConfig
  { typeArraysAsTuples   :: Bool
    -- ^  If set to @'True'@, each array will be considered a tuple of type
    --    @(a, b, c...)@ and the schema will be generated so that each member
    --    of the array-tuple will have its own type. If set to False, each
    --    array will be considered to have a single type @Array a@ and the
    --    schemas will be unified accordingly.
    --
    --    In terms of JSON Schema validation, setting this
    --    to @'True'@ will create an @items@ schema that is an array. Setting
    --    it to @'False'@ will create an @items@ schema that is an object.
  , sealObjectProperties :: Bool
    -- ^   If set to @'True'@, then each JSON object that the schema converter
    --     encounters will only be allowed to have all the keys present and
    --     no more. If set to @'False'@, then JSON objects that the schema converter
    --     encounters
    --
    --     In terms of JSON Schema validation, setting 'sealObjectProperties' to
    --     @'True'@ sets @additionalProperties: false@ on each schema object.
  , enumeratePrimitives  :: Bool
    -- ^   If set to @'True'@, then each primitive JSON __value__ (strings, integers,
    --     numbers, nulls, and booleans) will be explicitly specified as a valid
    --     value for that position in the schema. If set to @'False'@, then the
    --     __type__ of the value will be specified instead.
    --
    --     In terms of JSON Schema validation, setting 'enumeratePrimitives' to @'True'@ inserts
    --     the JSON value into the array of @enum@s.
  } deriving (Show)

{-| A configuration that can be used as a sane set of defaults when
   converting a JSON Schema into an object. It is used by default
   in 'JSONSchema.Draft4.SchemaGeneration.jsonToSchema' and
   'JSONSchema.Draft4.SchemaGeneration.jsonsToSchema'.
-}
defaultSchemaGenerationConfig :: SchemaGenerationConfig
defaultSchemaGenerationConfig =
  SchemaGenerationConfig {typeArraysAsTuples = False, sealObjectProperties = False, enumeratePrimitives = False}

instance Arbitrary SchemaGenerationConfig where
  arbitrary = sized f
    where
      f :: Int -> Gen SchemaGenerationConfig
      f n = do
        b1 <- arbitrary
        b2 <- arbitrary
        b3 <- arbitrary
        pure $ SchemaGenerationConfig b1 b2 b3

data SchemaUnificationConfig = SchemaUnificationConfig
  { enumerationLimit :: Maybe Int
    -- ^   If set to Nothing, then @enum@ values may accmuluate without limit when
    --     unifying schemas. If set to a @'Just'@ value, then when an @enum@ reaches
    --     the value of the integer contained within the Just, the entire @enum@
    --     array will be unified and merged with the original schema.
  } deriving (Show)

defaultSchemaUnificationConfig :: SchemaUnificationConfig
defaultSchemaUnificationConfig = SchemaUnificationConfig {enumerationLimit = Just 0}

instance Arbitrary SchemaUnificationConfig where
  arbitrary = sized f
    where
      f :: Int -> Gen SchemaUnificationConfig
      f n = do
        b1 <- oneof [pure Nothing, Just <$> arbitrary]
        pure $ SchemaUnificationConfig b1
