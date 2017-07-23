module JSONSchema.SchemaConverter
    (jsonToSchema, unifySchemas)
  where

import           Protolude hiding ((<>))

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

import qualified Safe as S
import qualified Data.Scientific as DS
import Data.Semigroup ((<>), Semigroup)

import Debug.Trace

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


{-
 Schema Unification.

 This is placed in the same file because it is expected we will have to
 perform mutual recursion with arrays of objects.
-}

unifySchemas :: D4.Schema -> D4.Schema -> D4.Schema
unifySchemas nextSchema =
    unifyPropertiesConstraint nextSchema
    . unifyRequiredConstraint nextSchema
    . unifyTypeConstraint nextSchema
    . unifyMaximumMinimumConstraints nextSchema
    . unifySimpleConstraints nextSchema


unifyMaybeList :: (Semigroup a) => [Maybe a] -> Maybe a
unifyMaybeList = S.foldr1May (<>) . catMaybes

-- Simple constraints are able to be enlarged without much complexity.
-- For example, if we have a schema with maximum 10 and a schema with maximum 20
-- the merged schema will have a maximum of 20.
unifySimpleConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifySimpleConstraints nextSchema accSchema = accSchema {
         D4._schemaMaxProperties = Utils.maxMaybe $ fmap D4._schemaMaxProperties schemas
       , D4._schemaMinProperties = Utils.minMaybe $ fmap D4._schemaMinProperties schemas
       , D4._schemaMaxItems = Utils.maxMaybe $ fmap D4._schemaMaxItems schemas
       , D4._schemaMinItems = Utils.minMaybe $ fmap D4._schemaMinItems schemas
       , D4._schemaMaxLength = Utils.maxMaybe $ fmap D4._schemaMaxLength schemas
       , D4._schemaMinLength = Utils.minMaybe $ fmap D4._schemaMinLength schemas
       , D4._schemaUniqueItems = Utils.andMaybe $ fmap D4._schemaUniqueItems schemas
    }
    where schemas = [nextSchema, accSchema]

-- We have no means of merging certain constraints that can only have
-- one opaque value (e.g. version) so we just pick one of them
-- unifyUniqueConstraints ::  D4.Schema -> D4.Schema -> D4.Schema
-- unifyUniqueConstraints nextSchema accSchema = accSchema {
--           D4._schemaVersion = listToMaybe . catMaybes $ fmap D4._schemaVersion schemas
--     }
--     where schemas = [nextSchema, accSchema]

unifyMaximumMinimumConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifyMaximumMinimumConstraints nextSchema accSchema =
    let schemas = [nextSchema, accSchema]
        maxes = fmap D4._schemaMaximum schemas
        emaxes = fmap D4._schemaExclusiveMaximum schemas
        mins = fmap D4._schemaMinimum schemas
        emins = fmap D4._schemaExclusiveMinimum schemas
        (maxConstraint, emaxConstraint) = Utils.computeMaximumConstraints maxes emaxes
        (minConstraint, eminConstraint) = Utils.computeMinimumConstraints mins emins
    in
        accSchema {
              D4._schemaMaximum = maxConstraint
            , D4._schemaExclusiveMaximum = emaxConstraint
            , D4._schemaMinimum = minConstraint
            , D4._schemaExclusiveMinimum = eminConstraint
        }

unifyTypeConstraint :: D4.Schema -> D4.Schema -> D4.Schema
unifyTypeConstraint nextSchema accSchema = accSchema {
        D4._schemaType =  S.foldr1May (<>) . catMaybes $ fmap D4._schemaType [nextSchema, accSchema]
    }

-- This should be set difference
unifyRequiredConstraint :: D4.Schema -> D4.Schema -> D4.Schema
unifyRequiredConstraint nextSchema accSchema = accSchema {
        D4._schemaRequired = S.foldr1May DS.intersection . catMaybes $ fmap D4._schemaRequired [nextSchema, accSchema]
    }

unifyPropertiesConstraint :: D4.Schema -> D4.Schema -> D4.Schema
unifyPropertiesConstraint nextSchema accSchema = accSchema {
        D4._schemaProperties = unifyProperties $ fmap D4._schemaProperties [nextSchema, accSchema]
    }
    where
        unifyProperties :: [Maybe (HM.HashMap Text D4.Schema)] -> Maybe (HM.HashMap Text D4.Schema)
        unifyProperties = S.foldr1May (HM.unionWith unifySchemas) . catMaybes
