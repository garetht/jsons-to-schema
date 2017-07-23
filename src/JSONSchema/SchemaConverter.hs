module JSONSchema.SchemaConverter
  ( jsonToSchema
  , jsonsToSchema
  , schemasToSchema
  , unifySchemas
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

makeBasicTypeSchema :: V4A.SchemaType -> D4.Schema
makeBasicTypeSchema t =
  D4.emptySchema {D4._schemaType = Just $ V4A.TypeValidatorString t}

makeObjectSchema :: AE.Object -> D4.Schema
makeObjectSchema o =
  (makeBasicTypeSchema V4A.SchemaObject) {
    D4._schemaRequired = requireds o,
    D4._schemaProperties = properties o
  }
    -- All keys encountered when there is just a single object are required
  where
    requireds = Just . DS.fromList . HM.keys
    properties = Just . map jsonToSchema

makeArrayAsTupleSchema :: AE.Array -> D4.Schema
makeArrayAsTupleSchema xs =
  (makeBasicTypeSchema V4A.SchemaArray) {
    D4._schemaItems = Just $ V4Arr.ItemsArray $ V.toList $ fmap jsonToSchema xs
  }

makeArrayAsSingleSchema :: AE.Array -> D4.Schema
makeArrayAsSingleSchema xs =
  (makeBasicTypeSchema V4A.SchemaArray) {
    D4._schemaItems =
      Just $ V4Arr.ItemsObject $ fromMaybe D4.emptySchema $ jsonsToSchema xs
  }

jsonToSchema :: AE.Value -> D4.Schema
jsonToSchema (AE.Number n) = makeBasicTypeSchema (if DS.isInteger n then V4A.SchemaInteger else V4A.SchemaNumber)
jsonToSchema (AE.String s) = makeBasicTypeSchema V4A.SchemaString
jsonToSchema (AE.Bool s)   = makeBasicTypeSchema V4A.SchemaBoolean
jsonToSchema AE.Null       = makeBasicTypeSchema V4A.SchemaNull
jsonToSchema (AE.Object o) = makeObjectSchema o
jsonToSchema (AE.Array xs) = makeArrayAsSingleSchema xs

schemasToSchema :: (Foldable f, Functor f) => f D4.Schema -> Maybe D4.Schema
schemasToSchema = SF.foldr1May unifySchemas

jsonsToSchema :: (Foldable f, Functor f) => f AE.Value -> Maybe D4.Schema
jsonsToSchema = schemasToSchema . fmap jsonToSchema
