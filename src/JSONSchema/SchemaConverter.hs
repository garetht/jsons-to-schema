module JSONSchema.SchemaConverter
  ( jsonToSchema
  , jsonToSchemaWithConfig
  , jsonsToSchema
  , jsonsToSchemaWithConfig
  , schemasToSchema
  , unifySchemas
  , SchemaGenerationConfig(..)
  , defaultSchemaGenerationConfig
  ) where

import           Protolude

import           JSONSchema.Unifiers               (unifySchemas)

import qualified Data.Aeson                        as AE
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.HashMap.Lazy                 as HM
import qualified Data.List.NonEmpty                as NE
import qualified Data.Set                          as DS
import qualified Data.Vector                       as V
import qualified JSONSchema.Draft4                 as D4

import qualified JSONSchema.Validator.Draft4.Any   as V4A
import qualified JSONSchema.Validator.Draft4.Array as V4Arr

import qualified Data.Scientific                   as DS
import qualified Safe                              as S
import qualified Safe.Foldable                     as SF

import qualified Utils

data SchemaGenerationConfig = SchemaGenerationConfig {
      typeArraysAsTuples :: Bool
    , sealObjectProperties :: Bool
}

defaultSchemaGenerationConfig :: SchemaGenerationConfig
defaultSchemaGenerationConfig = SchemaGenerationConfig {
    typeArraysAsTuples = False
  , sealObjectProperties = False
}

makeBasicTypeSchema :: V4A.SchemaType -> D4.Schema
makeBasicTypeSchema t =
  D4.emptySchema {D4._schemaType = Just $ V4A.TypeValidatorString t}

makeObjectSchema :: SchemaGenerationConfig -> AE.Object -> D4.Schema
makeObjectSchema c o =
  (makeBasicTypeSchema V4A.SchemaObject) {
    D4._schemaRequired = requireds o,
    D4._schemaProperties = properties o
  }
  where
    -- We add the required property only if the object has any keys
    requireds = fmap DS.fromList . Utils.listToMaybeList . HM.keys
    properties :: HM.HashMap Text AE.Value -> Maybe (HM.HashMap Text D4.Schema)
    properties = Just . map (jsonToSchemaWithConfig c)

makeArrayAsTupleSchema :: SchemaGenerationConfig -> AE.Array -> D4.Schema
makeArrayAsTupleSchema c xs =
  (makeBasicTypeSchema V4A.SchemaArray) {
    D4._schemaItems = Just $ V4Arr.ItemsArray $ V.toList $ fmap (jsonToSchemaWithConfig c) xs
  }

makeArrayAsSingleSchema :: SchemaGenerationConfig -> AE.Array -> D4.Schema
makeArrayAsSingleSchema c xs =
  (makeBasicTypeSchema V4A.SchemaArray) {
    D4._schemaItems =
      Just $ V4Arr.ItemsObject $ fromMaybe D4.emptySchema $ jsonsToSchemaWithConfig c xs
  }

jsonToSchemaWithConfig :: SchemaGenerationConfig -> AE.Value -> D4.Schema
jsonToSchemaWithConfig c (AE.Number n) = makeBasicTypeSchema (if DS.isInteger n then V4A.SchemaInteger else V4A.SchemaNumber)
jsonToSchemaWithConfig c (AE.String s) = makeBasicTypeSchema V4A.SchemaString
jsonToSchemaWithConfig c (AE.Bool s)   = makeBasicTypeSchema V4A.SchemaBoolean
jsonToSchemaWithConfig c AE.Null       = makeBasicTypeSchema V4A.SchemaNull
jsonToSchemaWithConfig c (AE.Object o) = makeObjectSchema c o
jsonToSchemaWithConfig c (AE.Array xs) = makeArrayAsSingleSchema c xs


jsonToSchema :: AE.Value -> D4.Schema
jsonToSchema = jsonToSchemaWithConfig defaultSchemaGenerationConfig

schemasToSchema :: (Foldable f, Functor f) => f D4.Schema -> Maybe D4.Schema
schemasToSchema = SF.foldr1May unifySchemas

jsonsToSchema :: (Foldable f, Functor f) => f AE.Value -> Maybe D4.Schema
jsonsToSchema = jsonsToSchemaWithConfig defaultSchemaGenerationConfig

jsonsToSchemaWithConfig :: (Foldable f, Functor f) => SchemaGenerationConfig -> f AE.Value -> Maybe D4.Schema
jsonsToSchemaWithConfig c = schemasToSchema . fmap (jsonToSchemaWithConfig defaultSchemaGenerationConfig)
