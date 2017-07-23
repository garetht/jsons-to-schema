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
import qualified JSONSchema.Validator.Draft4.Object as V4Obj

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
    unifyItemsConstraint nextSchema
    . unifyPropertiesConstraint nextSchema
    . unifyRequiredConstraint nextSchema
    . unifyTypeConstraint nextSchema
    . unifyMaximumMinimumConstraints nextSchema
    . unifySimpleConstraints nextSchema


-- The linear unifier extracts an array of Maybes and filters them
-- to only the Just values. We then use foldr1May to fold across the
-- list.. If there were no Just values, then
-- the foldr1 fails and we get Nothing. Otherwise, the present Just
-- values are folded together using foldF.
linearUnifier :: (a -> a -> a) -> (b -> Maybe a) -> [b] -> Maybe a
linearUnifier foldF getter xs = S.foldr1May foldF . catMaybes $ fmap getter xs

-- Simple constraints are able to be enlarged without much complexity.
-- For example, if we have a schema with maximum 10 and a schema with maximum 20
-- the merged schema will have a maximum of 20.
unifySimpleConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifySimpleConstraints nextSchema accSchema = accSchema {
         D4._schemaMaxProperties = linearUnifier max D4._schemaMaxProperties schemas
       , D4._schemaMinProperties = linearUnifier min D4._schemaMinProperties schemas
       , D4._schemaMaxItems = linearUnifier max D4._schemaMaxItems schemas
       , D4._schemaMinItems = linearUnifier min D4._schemaMinItems schemas
       , D4._schemaMaxLength = linearUnifier max D4._schemaMaxLength schemas
       , D4._schemaMinLength = linearUnifier min D4._schemaMinLength schemas
       , D4._schemaUniqueItems = linearUnifier (&&) D4._schemaUniqueItems schemas
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
        D4._schemaType =  linearUnifier (<>) D4._schemaType [nextSchema, accSchema]
    }

unifyRequiredConstraint :: D4.Schema -> D4.Schema -> D4.Schema
unifyRequiredConstraint nextSchema accSchema = accSchema {
        D4._schemaRequired = linearUnifier DS.intersection D4._schemaRequired [nextSchema, accSchema]
    }

unifyPropertiesConstraint :: D4.Schema -> D4.Schema -> D4.Schema
unifyPropertiesConstraint nextSchema accSchema = accSchema {
        D4._schemaProperties = linearUnifier (HM.unionWith unifySchemas) D4._schemaProperties [nextSchema, accSchema]
    }

unifyAdditionalPropertiesConstraint :: D4.Schema -> D4.Schema -> D4.Schema
unifyAdditionalPropertiesConstraint nextSchema accSchema = accSchema {
        D4._schemaAdditionalProperties = linearUnifier unifyAdditionalProperties D4._schemaAdditionalProperties [nextSchema, accSchema]
    }
    where unifyAdditionalProperties :: V4Obj.AdditionalProperties D4.Schema -> V4Obj.AdditionalProperties D4.Schema -> V4Obj.AdditionalProperties D4.Schema
          unifyAdditionalProperties (V4Obj.AdditionalPropertiesBool b1) (V4Obj.AdditionalPropertiesBool b2) = V4Obj.AdditionalPropertiesBool $ b1 || b2
          -- allowing additional objects (True) is always at least as permissive as any schema
          -- all schemas are at least as permissive as not allowing any additional objects (False)
          unifyAdditionalProperties bool@(V4Obj.AdditionalPropertiesBool b) obj@(V4Obj.AdditionalPropertiesObject s) = if b then bool else obj
          unifyAdditionalProperties obj@(V4Obj.AdditionalPropertiesObject s) bool@(V4Obj.AdditionalPropertiesBool b) = if b then bool else obj
          unifyAdditionalProperties (V4Obj.AdditionalPropertiesObject o1) (V4Obj.AdditionalPropertiesObject o2) = V4Obj.AdditionalPropertiesObject $ unifySchemas o1 o2

unifyItemsConstraint :: D4.Schema -> D4.Schema -> D4.Schema
unifyItemsConstraint nextSchema accSchema = accSchema {
        D4._schemaItems = linearUnifier unifyItems D4._schemaItems [nextSchema, accSchema]
    }
    where unifyItems :: V4Arr.Items D4.Schema -> V4Arr.Items D4.Schema -> V4Arr.Items D4.Schema
          unifyItems (V4Arr.ItemsObject o1) (V4Arr.ItemsObject o2) = V4Arr.ItemsObject $ unifySchemas o1 o2
          unifyItems (V4Arr.ItemsArray xs) (V4Arr.ItemsArray ys) =
            V4Arr.ItemsArray $ fmap (uncurry unifySchemas) (Utils.zipWithPadding D4.emptySchema D4.emptySchema xs ys)
          unifyItems x y = x -- possibly: merge the object schema into each of the tuple schemas? (zip (repeat o1) xs)

unifyAdditionalItemsConstraint :: D4.Schema -> D4.Schema -> D4.Schema
unifyAdditionalItemsConstraint nextSchema accSchema = accSchema {
        D4._schemaAdditionalItems = linearUnifier unifyAdditionalItems D4._schemaAdditionalItems [nextSchema, accSchema]
    }
    where unifyAdditionalItems :: V4Arr.AdditionalItems D4.Schema -> V4Arr.AdditionalItems D4.Schema -> V4Arr.AdditionalItems D4.Schema
          unifyAdditionalItems (V4Arr.AdditionalBool b1) (V4Arr.AdditionalBool b2) = V4Arr.AdditionalBool $ b1 || b2
          -- allowing additional objects (True) is always at least as permissive as any schema
          -- all schemas are at least as permissive as not allowing any additional objects (False)
          unifyAdditionalItems bool@(V4Arr.AdditionalBool b) obj@(V4Arr.AdditionalObject s) = if b then bool else obj
          unifyAdditionalItems obj@(V4Arr.AdditionalObject s) bool@(V4Arr.AdditionalBool b) = if b then bool else obj
          unifyAdditionalItems (V4Arr.AdditionalObject o1) (V4Arr.AdditionalObject o2) = V4Arr.AdditionalObject $ unifySchemas o1 o2
