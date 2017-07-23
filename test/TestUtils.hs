module TestUtils
  ( testJsonToSchema
  , testJsonsToSchema
  ) where

import Protolude

import qualified Data.Aeson as AE
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

parseJson :: Text -> AE.Value
parseJson json =
  (fromMaybe (panic $ "Could not parse JSON " <> json) .
   AE.decode . BSL.fromStrict . TE.encodeUtf8)
    json

testJsonToSchema :: Text -> Text -> IO ()
testJsonToSchema jsonText expectedSchema =
  (jsonToSchema . parseJson $ jsonText) `shouldBe` parseSchema expectedSchema

testJsonsToSchema :: [Text] -> Text -> IO ()
testJsonsToSchema jsonTexts expectedSchema =
  fromMaybe
    (panic "Could not unify multiple schemas")
    (jsonsToSchema $ fmap parseJson jsonTexts) `shouldBe`
  parseSchema expectedSchema
