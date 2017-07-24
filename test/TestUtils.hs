module TestUtils
  ( testJsonToSchema
  , testJsonsToSchema
  , testJsonsToSchemaPretty
  ) where

import Protolude

import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AEEP
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified JSONSchema.Draft4 as D4

import JSONSchema.SchemaConverter
import Test.Hspec

import qualified GHC.Base

parseSchema :: Text -> D4.Schema
parseSchema =
  fromMaybe (panic "Failed to parse schema") .
  AE.decode . BSL.fromStrict . TE.encodeUtf8

printSchema :: D4.Schema -> BSL.ByteString
printSchema = AEEP.encodePretty . AE.toJSON

parseJson :: Text -> AE.Value
parseJson json =
  (fromMaybe (panic $ "Could not parse JSON " <> json) .
   AE.decode . BSL.fromStrict . TE.encodeUtf8)
    json

testJsonToSchema :: Text -> Text -> IO ()
testJsonToSchema jsonText = testJsonsToSchema [jsonText]

testJsonsToSchema :: [Text] -> Text -> IO ()
testJsonsToSchema jsonTexts expectedSchema = do
  let jsonInstances = fmap parseJson jsonTexts
  let computedSchema = fromMaybe
                   (panic "Could not unify multiple schemas")
                   (jsonsToSchema jsonInstances)

  -- Check schema is as expected
  computedSchema `shouldBe` parseSchema expectedSchema

  -- Check all provided instances validate against the computed schema
  let validatableSchema = D4.SchemaWithURI computedSchema Nothing
  results <- sequence $ fmap (D4.fetchFilesystemAndValidate validatableSchema) jsonInstances

  all isRight results `shouldBe` True


testJsonsToSchemaPretty :: [Text] -> Text -> IO ()
testJsonsToSchemaPretty jsonTexts expectedSchema =
  maybe
    (panic "Could not unify multiple schemas")
    printSchema
    (jsonsToSchema $ fmap parseJson jsonTexts) `shouldBe`
  printSchema (parseSchema expectedSchema)