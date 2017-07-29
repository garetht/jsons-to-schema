{-|
  Module      : JSONSchema.Draft4.SchemaGeneration
  Description : Generates schemas from JSON and other schemas
  Copyright   : (c) Gareth Tan, 2017
  License     : GPL-3

  A single JSON document can be interpreted multiple ways and
  with differing degrees of strictness when it is converted
  into a JSON Schema. These options allow the way in which a
  JSON document is converted into a schema to be customized.
-}

module JSONSchema.Draft4.SchemaGeneration
  ( jsonToSchema
  , jsonToSchemaWithConfig
  , jsonsToSchema
  , jsonsToSchemaWithConfig
  , schemasToSchema
  , unifySchemas
  ) where

import           Protolude

import           JSONSchema.Draft4.SchemaUnification      (unifySchemas)

import qualified Data.Aeson                               as AE
import qualified Data.HashMap.Lazy                        as HM
import qualified Data.Scientific                          as DSC
import qualified Data.Set                                 as DST
import qualified Data.Vector                              as V
import qualified JSONSchema.Draft4                        as D4

import qualified JSONSchema.Validator.Draft4.Any          as V4A
import qualified JSONSchema.Validator.Draft4.Array        as V4Arr
import qualified JSONSchema.Validator.Draft4.Object       as V4Obj

import qualified Safe.Foldable                            as SF

import           JSONSchema.Draft4.SchemaGenerationConfig
import qualified JSONSchema.Draft4.Internal.Utils as Utils


makeBasicTypeSchema :: V4A.SchemaType -> D4.Schema
makeBasicTypeSchema t =
  D4.emptySchema {D4._schemaType = Just $ V4A.TypeValidatorString t}

makeObjectSchema :: SchemaGenerationConfig -> AE.Object -> D4.Schema
makeObjectSchema c o =
  (makeBasicTypeSchema V4A.SchemaObject) {
      D4._schemaRequired = requireds o
    , D4._schemaProperties = properties o
    , D4._schemaAdditionalProperties = additionalProperties
  }
  where
    -- We add the required property only if an object has keys. Generating
    -- required: [] will not work because the Draft 4 specification does not
    -- permit empty arrays. Instead, when unifying the required type, we
    -- make sure that a _schemaRequired of Nothing will eliminate all other
    -- specified required properties, i.e. if an object does not require any
    -- properties, than no other object can either.
    requireds = fmap DST.fromList . Utils.listToMaybeList . HM.keys
    properties :: HM.HashMap Text AE.Value -> Maybe (HM.HashMap Text D4.Schema)
    properties = Just . map (jsonToSchemaWithConfig c)
    -- Make objects unable to accept additional properties if specified by the user
    additionalProperties = if sealObjectProperties c
      then Just $ V4Obj.AdditionalPropertiesBool False
      else Nothing

makeArrayAsTupleSchema :: SchemaGenerationConfig -> AE.Array -> D4.Schema
makeArrayAsTupleSchema c xs =
  (makeBasicTypeSchema V4A.SchemaArray) {
    -- the inner Maybe checks to see if the array is empty
    --  because "items": [] is not valid according to the metaschema
    -- Under 5.3.1.4.  Default values the absence of `items` is equivalent
    -- to "items": {}
    D4._schemaItems = createItemsArray xs
  }
    where
      createItemsArray :: V.Vector AE.Value -> Maybe (V4Arr.Items D4.Schema)
      createItemsArray =
        (V4Arr.ItemsArray <$>) .
        Utils.listToMaybeList .
        V.toList .
        fmap (jsonToSchemaWithConfig c)

makeArrayAsSingleSchema :: SchemaGenerationConfig -> AE.Array -> D4.Schema
makeArrayAsSingleSchema c xs =
  (makeBasicTypeSchema V4A.SchemaArray) {
    D4._schemaItems =
      Just $ V4Arr.ItemsObject $ fromMaybe D4.emptySchema $ jsonsToSchemaWithConfig c xs
  }

{-| Converts a single JSON document into a JSON schema which the document
    will validate against. Configuration options allow customizing how
    the original JSON document should be interpreted when it is converted
    into a schema.

    The options provided control how even recursively nested schemas will
    be interpreted. There is currently no option to interpret a JSON
    document one way given one path and another way at another path.
-}
jsonToSchemaWithConfig :: SchemaGenerationConfig -> AE.Value -> D4.Schema
jsonToSchemaWithConfig c (AE.Number n) = makeBasicTypeSchema (if DSC.isInteger n then V4A.SchemaInteger else V4A.SchemaNumber)
jsonToSchemaWithConfig c (AE.String s) = makeBasicTypeSchema V4A.SchemaString
jsonToSchemaWithConfig c (AE.Bool s)   = makeBasicTypeSchema V4A.SchemaBoolean
jsonToSchemaWithConfig c AE.Null       = makeBasicTypeSchema V4A.SchemaNull
jsonToSchemaWithConfig c (AE.Object o) = makeObjectSchema c o
jsonToSchemaWithConfig c (AE.Array xs) = arrayConverter c xs
  where arrayConverter = if typeArraysAsTuples c then makeArrayAsTupleSchema else makeArrayAsSingleSchema

{-| Converts a single JSON document into a JSON schema which the
    document will validate against by using the default options specified
    in 'defaultSchemaGenerationConfig'.
-}
jsonToSchema :: AE.Value -> D4.Schema
jsonToSchema = jsonToSchemaWithConfig defaultSchemaGenerationConfig

{-| Combines multiple schemas into a single schema by folding across them using
    'unifySchemas'. The 'Maybe' accounts for the case where it is passed
    an empty 'Foldable'.
-}
schemasToSchema :: (Foldable f, Functor f) => f D4.Schema -> Maybe D4.Schema
schemasToSchema = SF.foldr1May unifySchemas

{-| Combines multiple JSON documents into a single schema by first converting each into a schema and
    folding across the schemas using 'unifySchemas'. The documents are converted into schemas using
    the default options specified in 'defaultSchemaGenerationConfig'.
-}
jsonsToSchema :: (Foldable f, Functor f) => f AE.Value -> Maybe D4.Schema
jsonsToSchema = jsonsToSchemaWithConfig defaultSchemaGenerationConfig

{-| Combines multiple JSON documents into a single schema by first converting each into a schema and
    folding across the schemas using 'unifySchemas'. This allows a specific configuration for parsing
    the JSON documents into the schemas to be provided.
-}
jsonsToSchemaWithConfig :: (Foldable f, Functor f) => SchemaGenerationConfig -> f AE.Value -> Maybe D4.Schema
jsonsToSchemaWithConfig c = schemasToSchema . fmap (jsonToSchemaWithConfig c)
