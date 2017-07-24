{-# LANGUAGE QuasiQuotes #-}

module JSONSchema.QuickCheckTests where

import           Protolude

import qualified JSONSchema.Draft4                 as D4
import           JSONSchema.SchemaGenerationConfig
import           Test.Hspec
import Test.QuickCheck

import           JSONSchema.Unifiers               as JU

tupleTypedArrayConfig = defaultSchemaGenerationConfig {
  typeArraysAsTuples = True
}

sealedObjectPropertiesConfig = defaultSchemaGenerationConfig {
  sealObjectProperties = True
}

-- N.B. it is in general not true that the empty schema acts as the identity
-- value with regards to unifySchema because the empty schema represents a
-- more relaxed value for exclusiveMinimum and exclusiveMaximum, which we
-- must respect.

testPropUnifyRequiredMonotonicallyDecreasing :: Spec
testPropUnifyRequiredMonotonicallyDecreasing = undefined

-- testPropUnifyEmptySchemaRightIdentity :: Spec
-- testPropUnifyEmptySchemaRightIdentity = it "will not change a schema when an empty schema is passed in on the right" $
--   property prop
--   where prop s = JU.unifySchemas (limit s) D4.emptySchema == limit s
--         limit s = s {D4._schemaExclusiveMinimum = Just False, D4._schemaExclusiveMaximum = Just False}
