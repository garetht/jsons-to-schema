module JSONSchema.SchemaGenerationConfig
  ( SchemaGenerationConfig(..)
  , defaultSchemaGenerationConfig
  ) where

import           Protolude
import           Test.QuickCheck (Arbitrary, Gen, sized, arbitrary)


data SchemaGenerationConfig = SchemaGenerationConfig {
    -- If set to True, each array will be considered a tuple, instead
    -- of the entire array having a single type (i.e. Array<T>)
    typeArraysAsTuples   :: Bool
    -- If set to True, then when generating an object schema
    -- additionalProperties will be set to `false`, disallowing
    -- objects with additional properties to be validated against
    -- the same schema
  , sealObjectProperties :: Bool
} deriving (Show)

defaultSchemaGenerationConfig :: SchemaGenerationConfig
defaultSchemaGenerationConfig = SchemaGenerationConfig {
    typeArraysAsTuples = False
  , sealObjectProperties = False
}

instance Arbitrary SchemaGenerationConfig where
  arbitrary = sized f
    where
      f :: Int -> Gen SchemaGenerationConfig
      f n = do
        b1 <- arbitrary
        b2 <- arbitrary
        pure $ SchemaGenerationConfig b1 b2
