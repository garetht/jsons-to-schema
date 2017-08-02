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
module JSONSchema.Draft4.SchemaGenerationConfig
  ( SchemaGenerationConfig(..)
  , defaultSchemaGenerationConfig
  ) where

import           Protolude
import           Test.QuickCheck (Arbitrary, Gen, arbitrary, sized)

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
  } deriving (Show)

{-| A configuration that can be used as a sane set of defaults when
   converting a JSON Schema into an object. It is used by default
   in 'JSONSchema.Draft4.SchemaGeneration.jsonToSchema' and
   'JSONSchema.Draft4.SchemaGeneration.jsonsToSchema'.
-}
defaultSchemaGenerationConfig :: SchemaGenerationConfig
defaultSchemaGenerationConfig = SchemaGenerationConfig {typeArraysAsTuples = False, sealObjectProperties = False}

instance Arbitrary SchemaGenerationConfig where
  arbitrary = sized f
    where
      f :: Int -> Gen SchemaGenerationConfig
      f n = do
        b1 <- arbitrary
        b2 <- arbitrary
        pure $ SchemaGenerationConfig b1 b2
