module JSONSchema.Draft4.SchemaUnification
  ( unifySchemas
  ) where

import           Protolude                               hiding ((<>))

import qualified Data.HashMap.Lazy                       as HM
import qualified Data.Set                                as DS
import qualified JSONSchema.Draft4                       as D4

import qualified JSONSchema.Validator.Draft4.Array       as V4Arr
import qualified JSONSchema.Validator.Draft4.Object      as V4Obj

import           Data.Semigroup                          ((<>))
import qualified Safe                                    as S

import qualified JSONSchema.Draft4.Internal.Utils as Utils

unifySchemas :: D4.Schema -> D4.Schema -> D4.Schema
unifySchemas nextSchema =
  unifyObjectConstraints nextSchema .
  unifyArrayConstraints nextSchema .

  unifyStringConstraints nextSchema .
  unifyNumericConstraints nextSchema .

  unifyAnyInstanceConstraints nextSchema .
  unifyNonvalidatingConstraints nextSchema

-- The linear unifier extracts an array of Maybes and filters them
-- to only the Just values. We then use foldr1May to fold across the
-- list.. If there were no Just values, then
-- the foldr1 fails and we get Nothing. Otherwise, the present Just
-- values are folded together using foldF.

--The linear unifier applies the binary function if both are Just, returns the identity
-- if only one is Just, and Nothing if both are Nothing.
linearUnifier :: (a -> a -> a) -> (b -> Maybe a) -> b -> b -> Maybe a
linearUnifier foldF getter next acc = S.foldr1May foldF . catMaybes $ fmap getter [next, acc]

-- The applicative unifier applies the binary function but returns Nothing if either
-- is Nothing in the manner of an applicative.
applicativeUnifier :: (a -> a -> a) -> (b -> Maybe a) -> b -> b -> Maybe a
applicativeUnifier binF getter next acc = binF <$> getter next <*> getter acc


unifyNonvalidatingConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifyNonvalidatingConstraints nextSchema accSchema = accSchema {
      D4._schemaVersion = linearUnifier const D4._schemaVersion nextSchema accSchema
    , D4._schemaId = linearUnifier const D4._schemaId nextSchema accSchema
    , D4._schemaRef = linearUnifier const D4._schemaRef nextSchema accSchema
    , D4._schemaDefinitions = linearUnifier const D4._schemaDefinitions nextSchema accSchema
    , D4._schemaDependencies = linearUnifier const D4._schemaDependencies nextSchema accSchema
    , D4._schemaOther = foldr HM.union HM.empty (fmap D4._schemaOther [nextSchema, accSchema])
  }

unifyNumericConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifyNumericConstraints nextSchema accSchema =
  accSchema
       { D4._schemaMaximum = maxConstraint
       , D4._schemaExclusiveMaximum = emaxConstraint
       , D4._schemaMinimum = minConstraint
       , D4._schemaExclusiveMinimum = eminConstraint
       , D4._schemaMultipleOf = linearUnifier const D4._schemaMultipleOf nextSchema accSchema
       }
  where
    schemas = [nextSchema, accSchema]
    maxes = fmap D4._schemaMaximum schemas
    emaxes = fmap D4._schemaExclusiveMaximum schemas
    mins = fmap D4._schemaMinimum schemas
    emins = fmap D4._schemaExclusiveMinimum schemas
    (maxConstraint, emaxConstraint) =
      Utils.computeMaximumConstraints maxes emaxes
    (minConstraint, eminConstraint) =
      Utils.computeMinimumConstraints mins emins

unifyStringConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifyStringConstraints nextSchema accSchema = accSchema {
      D4._schemaMaxLength = linearUnifier max D4._schemaMaxLength nextSchema accSchema
    , D4._schemaMinLength = linearUnifier min D4._schemaMinLength nextSchema accSchema
    , D4._schemaPattern = linearUnifier const D4._schemaPattern nextSchema accSchema
  }

unifyArrayConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifyArrayConstraints nextSchema accSchema = accSchema {
      D4._schemaItems = linearUnifier unifyItems D4._schemaItems nextSchema accSchema
    , D4._schemaAdditionalItems = linearUnifier unify D4._schemaAdditionalItems nextSchema accSchema
    , D4._schemaMaxItems = linearUnifier max D4._schemaMaxItems nextSchema accSchema
    , D4._schemaMinItems = linearUnifier min D4._schemaMinItems nextSchema accSchema
    , D4._schemaUniqueItems = linearUnifier (&&) D4._schemaUniqueItems nextSchema accSchema
  }
  where
    unify :: V4Arr.AdditionalItems D4.Schema -> V4Arr.AdditionalItems D4.Schema -> V4Arr.AdditionalItems D4.Schema
    unify (V4Arr.AdditionalBool b1) (V4Arr.AdditionalBool b2) =
      V4Arr.AdditionalBool $ b1 || b2
    -- allowing additional objects (True) is always at least as permissive as any schema
    -- all schemas are at least as permissive as not allowing any additional objects (False)
    unify bln@(V4Arr.AdditionalBool b) obj@(V4Arr.AdditionalObject s) = if b then bln else obj
    unify obj@(V4Arr.AdditionalObject s) bln@(V4Arr.AdditionalBool b) = if b then bln else obj
    unify (V4Arr.AdditionalObject o1) (V4Arr.AdditionalObject o2) =
      V4Arr.AdditionalObject $ unifySchemas o1 o2

    unifyItems :: V4Arr.Items D4.Schema -> V4Arr.Items D4.Schema -> V4Arr.Items D4.Schema
    unifyItems (V4Arr.ItemsObject o1) (V4Arr.ItemsObject o2) = V4Arr.ItemsObject $ unifySchemas o1 o2
    unifyItems (V4Arr.ItemsArray xs) (V4Arr.ItemsArray ys) = V4Arr.ItemsArray $
      fmap (uncurry unifySchemas) (Utils.zipWithPadding D4.emptySchema D4.emptySchema xs ys)
    unifyItems x y = x -- possibly: merge the object schema into each of the tuple schemas? (zip (repeat o1) xs)

-- If one object does not require properties, then none of them can require
-- any properties. If the `required` array would be empty, we return Nothing
-- instead (the V4 metaschema says the required array cannot be empty)
unifyObjectConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifyObjectConstraints nextSchema accSchema = accSchema {
      D4._schemaRequired = Utils.setToMaybeSet =<< -- to avoid empty lists, which are not allowed
        applicativeUnifier DS.intersection D4._schemaRequired nextSchema accSchema
    , D4._schemaProperties =
            linearUnifier (HM.unionWith unifySchemas) D4._schemaProperties nextSchema accSchema
    , D4._schemaAdditionalProperties =
           linearUnifier unify D4._schemaAdditionalProperties nextSchema accSchema
    , D4._schemaMaxProperties = linearUnifier max D4._schemaMaxProperties nextSchema accSchema
    , D4._schemaMinProperties = linearUnifier min D4._schemaMinProperties nextSchema accSchema
    , D4._schemaPatternProperties = linearUnifier HM.union D4._schemaPatternProperties nextSchema accSchema
  }
  where
    unify :: V4Obj.AdditionalProperties D4.Schema -> V4Obj.AdditionalProperties D4.Schema -> V4Obj.AdditionalProperties D4.Schema
    unify (V4Obj.AdditionalPropertiesBool b1) (V4Obj.AdditionalPropertiesBool b2) =
      V4Obj.AdditionalPropertiesBool $ b1 || b2
    -- allowing additional objects (True) is always at least as permissive as any schema
    -- all schemas are at least as permissive as not allowing any additional objects (False)
    unify bln@(V4Obj.AdditionalPropertiesBool b) obj@(V4Obj.AdditionalPropertiesObject s) = if b then bln else obj
    unify obj@(V4Obj.AdditionalPropertiesObject s) bln@(V4Obj.AdditionalPropertiesBool b) = if b then bln else obj
    unify (V4Obj.AdditionalPropertiesObject o1) (V4Obj.AdditionalPropertiesObject o2) =
      V4Obj.AdditionalPropertiesObject $ unifySchemas o1 o2

unifyAnyInstanceConstraints :: D4.Schema -> D4.Schema -> D4.Schema
unifyAnyInstanceConstraints nextSchema accSchema = accSchema {
      D4._schemaType = linearUnifier (<>) D4._schemaType nextSchema accSchema
    , D4._schemaEnum = linearUnifier const D4._schemaEnum nextSchema accSchema
    , D4._schemaAllOf = linearUnifier const D4._schemaAllOf nextSchema accSchema
    , D4._schemaAnyOf = linearUnifier const D4._schemaAnyOf nextSchema accSchema
    , D4._schemaOneOf = linearUnifier const D4._schemaOneOf nextSchema accSchema
    , D4._schemaNot = linearUnifier const D4._schemaNot nextSchema accSchema
  }
