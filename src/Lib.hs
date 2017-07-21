{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  (someFunc)
  where

import Protolude
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AEEP
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as DS
import qualified JSONSchema.Draft4 as D4
import qualified Data.Vector as V

import qualified JSONSchema.Validator.Draft4.Any as V4A
import qualified JSONSchema.Validator.Draft4.Array as V4Arr

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

schemaUnifier :: D4.Schema -> D4.Schema -> D4.Schema
schemaUnifier = undefined

someData :: AE.Value
someData = AE.Object $ HM.fromList [("test", AE.String "hello"), ("again", AE.Object $ HM.fromList [("test", AE.String "hello"), ("again", AE.Number 14)])]

someFunc :: IO ()
someFunc = putStrLn $ AEEP.encodePretty $ AE.toJSON $ jsonToSchema someData
