module JSONSchema.SchemaGenerationConfig
  ( SchemaGenerationConfig(..)
  , defaultSchemaGenerationConfig
  ) where

import Protolude

data SchemaGenerationConfig = SchemaGenerationConfig {
    typeArraysAsTuples :: Bool
  , sealObjectProperties :: Bool
}

defaultSchemaGenerationConfig :: SchemaGenerationConfig
defaultSchemaGenerationConfig = SchemaGenerationConfig {
    typeArraysAsTuples = False
  , sealObjectProperties = False
}
