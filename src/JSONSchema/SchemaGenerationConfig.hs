module JSONSchema.SchemaGenerationConfig
  ( SchemaGenerationConfig(..)
  , defaultSchemaGenerationConfig
  ) where

import Protolude

data SchemaGenerationConfig = SchemaGenerationConfig {
    -- If set to True,
    typeArraysAsTuples :: Bool
    -- If set to True, then when generating an object schema
    -- additionalProperties will be set to `false`, disallowing
    -- objects with additional properties to be validated against
    -- the same schema
  , sealObjectProperties :: Bool
}

defaultSchemaGenerationConfig :: SchemaGenerationConfig
defaultSchemaGenerationConfig = SchemaGenerationConfig {
    typeArraysAsTuples = False
  , sealObjectProperties = False
}
