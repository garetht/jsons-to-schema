module JSONSchema.SchemaConverter
    (jsonToSchema)
  where

import           Protolude

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

import qualified Utils

makeBasicTypeSchema :: V4A.SchemaType -> D4.Schema
makeBasicTypeSchema t =
    D4.emptySchema { D4._schemaType = Just $ V4A.TypeValidatorString t }

makeObjectSchema :: AE.Object -> D4.Schema
makeObjectSchema o =
    (makeBasicTypeSchema V4A.SchemaObject) {
        D4._schemaRequired = requireds o
      , D4._schemaProperties = properties o
    }
  where
    -- All keys encountered when there is just a single object are required
    requireds = Just . DS.fromList . HM.keys
    properties = Just . map jsonToSchema

makeArrayAsTupleSchema :: AE.Array -> D4.Schema
makeArrayAsTupleSchema xs = (makeBasicTypeSchema V4A.SchemaArray) {
    D4._schemaItems = Just $ V4Arr.ItemsArray $ V.toList $ fmap jsonToSchema xs
}

jsonToSchema :: AE.Value -> D4.Schema
jsonToSchema (AE.Number n) = makeBasicTypeSchema V4A.SchemaNumber
jsonToSchema (AE.String s) = makeBasicTypeSchema V4A.SchemaString
jsonToSchema (AE.Bool s) = makeBasicTypeSchema V4A.SchemaBoolean
jsonToSchema AE.Null = makeBasicTypeSchema V4A.SchemaNull
jsonToSchema (AE.Object o) = makeObjectSchema o
jsonToSchema (AE.Array xs) = makeArrayAsTupleSchema xs

unifySchemas :: [D4.Schema] -> D4.Schema
unifySchemas = undefined

-- If neither has a type, then just merge the two schemas together
-- without any further thought
schemaUnifier :: D4.Schema -> D4.Schema -> D4.Schema
schemaUnifier nextSchema accSchema = D4.emptySchema {
         D4._schemaMaxProperties = Utils.maxMaybe $ fmap D4._schemaMaxProperties schemas
       , D4._schemaMinProperties = Utils.minMaybe $ fmap D4._schemaMinProperties schemas
       , D4._schemaMaxItems = Utils.maxMaybe $ fmap D4._schemaMaxItems schemas
       , D4._schemaMinItems = Utils.minMaybe $ fmap D4._schemaMinItems schemas
       , D4._schemaMaximum = Utils.maxMaybe $ fmap D4._schemaMaximum schemas
       , D4._schemaMinimum = Utils.minMaybe $ fmap D4._schemaMinimum schemas
       , D4._schemaMaxLength = Utils.maxMaybe $ fmap D4._schemaMaxLength schemas
       , D4._schemaMinLength = Utils.minMaybe $ fmap D4._schemaMinLength schemas
    }
    where schemas = [nextSchema, accSchema]
