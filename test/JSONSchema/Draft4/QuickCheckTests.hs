module JSONSchema.Draft4.QuickCheckTests where

import           Protolude

import qualified Data.Aeson                               as AE
import           JSONSchema.Draft4

import qualified JSONSchema.Draft4                        as D4
import           JSONSchema.Draft4.SchemaGeneration       as JSSC
import           JSONSchema.Draft4.SchemaGenerationConfig

import qualified GHC.Base
import           JSONSchema.Draft4.QuickCheckInstances
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances                ()
import           TestUtils

tupleTypedArrayConfig :: SchemaGenerationConfig
tupleTypedArrayConfig = defaultSchemaGenerationConfig {typeArraysAsTuples = True}

sealedObjectPropertiesConfig :: SchemaGenerationConfig
sealedObjectPropertiesConfig = defaultSchemaGenerationConfig {sealObjectProperties = True}

explainSchemaCounterexample :: [AE.Value] -> D4.Schema -> SchemaGenerationConfig -> GHC.Base.String
explainSchemaCounterexample jsons schema config =
  "The JSONs " <> foldr (<>) "" (fmap printJsonToString jsons) <> " do not all validate against their generated schema " <>
  printSchemaToString schema <>
  " when run with configuration " <>
  show config

explainSchemaCommutative :: D4.Schema -> D4.Schema -> GHC.Base.String
explainSchemaCommutative s1 s2 =
  "These schemas could not be united commutatively:" <> printSchemaToString s1 <> printSchemaToString s2 <>
  "When s1 was unified into s2 the result was \n" <>
  printSchemaToString (unifySchemas s1 s2) <>
  " but when s2 was unified into s1 the result was\n " <>
  printSchemaToString (unifySchemas s2 s1)

explainSchemaSelfUnify :: D4.Schema -> GHC.Base.String
explainSchemaSelfUnify s =
  "This schema did not properly unify with itself:" <> printSchemaToString s <>
  "When it was unified with itself it should have stayed the same but the result instead was \n" <>
  printSchemaToString (unifySchemas s s)

testPropUnifyEmptySchemaRightIdentity :: Spec
testPropUnifyEmptySchemaRightIdentity =
  prop "will not change a restricted schema when an empty schema is passed in on the right" p
  where
    p :: RestrictedSchema -> Bool
    p rs = JSSC.unifySchemas (getSchema rs) emptySchema == getSchema rs

testPropUnifyEmptySchemaLeftIdentity :: Spec
testPropUnifyEmptySchemaLeftIdentity =
  prop "will not change a restricted schema when an empty schema is passed in on the left" p
  where
    p :: RestrictedSchema -> Bool
    p rs = JSSC.unifySchemas emptySchema (getSchema rs) == getSchema rs

-- Unable yet to generate recursively commutative schemas
testSchemaUnificationCommutative :: Spec
testSchemaUnificationCommutative =
  modifyMaxSize (const 1) $
  prop "schema unification of a schema with non-const properties is commutative" propUnificationCommutative
  where
    propUnificationCommutative :: CommutativeSchema -> CommutativeSchema -> Property
    propUnificationCommutative r1 r2 =
      counterexample (explainSchemaCommutative s1 s2) (unifySchemas s1 s2 == unifySchemas s2 s1)
      where
        s1 = getCommutativeSchema r1
        s2 = getCommutativeSchema r2

-- Unable yet to generate nested schemas without `required`, which must not be empty.
testSchemaUnifiedWithSelfIsSelf :: Spec
testSchemaUnifiedWithSelfIsSelf =
  modifyMaxSize (const 1) $ prop "when a schema is unified with itself it does not change" propSelfUnification
  where
    propSelfUnification :: RestrictedSchema -> Property
    propSelfUnification s = counterexample (explainSchemaSelfUnify rs) (unifySchemas rs rs == rs)
      where
        rs = getSchema s

testJsonToSchemaWithConfigValidatesJson :: Spec
testJsonToSchemaWithConfigValidatesJson =
  prop
    "will generate a schema that can validate the JSON used to generate the schema with a randomized configuration"
    configurer
  where
    configurer :: SchemaGenerationConfig -> Property
    configurer config = sizedJsonProp 7 (p config)
    p :: SchemaGenerationConfig -> AE.Value -> Property
    p config json = counterexample (explainSchemaCounterexample [json] schema config) (schema `validatesAll` [json])
      where
        schema = JSSC.jsonToSchemaWithConfig config json

testSchemaUnificationValidatesAllJson :: Spec
testSchemaUnificationValidatesAllJson =
  prop "will generate a schema that validates all the JSON documents unified to produce it" configurer
  where
    configurer :: SchemaGenerationConfig -> Property
    configurer config = sizedJsonsProp 7 (p config)
    p :: SchemaGenerationConfig -> [AE.Value] -> Property
    p config jsons = counterexample (explainSchemaCounterexample jsons schema config) (schema `validatesAll` jsons)
      where
        schema = fromMaybe (panic "Could not parse schemas") (JSSC.jsonsToSchemaWithConfig config jsons)
