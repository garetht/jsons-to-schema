module TestUtils
  ( testJsonToSchema
  , testJsonsToSchema
  , testJsonsToSchemaWithConfig
  , testJsonsToSchemaPretty
  , testJsonsToSchemaPrettyWithConfig
  , shouldValidate
  , shouldNotValidate
  , shouldNotValidateTexts
  , validatesAll
  , testUnifySchemas
  , testUnifySchemaTexts
  , printJson
  , printJsonToString
  , printSchemaToString
  ) where

import           Protolude

import qualified Data.Aeson                        as AE
import qualified Data.Aeson.Encode.Pretty          as AEEP
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Lazy                 as HM
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import qualified JSONSchema.Draft4                 as D4

import           JSONSchema.SchemaConverter
import           JSONSchema.SchemaGenerationConfig
import           Test.Hspec

import qualified GHC.Base

parseSchema :: Text -> D4.Schema
parseSchema s =
  fromMaybe (panic $ "Failed to parse schema " <> s) .
  AE.decode .
  BSL.fromStrict .
  TE.encodeUtf8 $ s

printSchema :: D4.Schema -> BSL.ByteString
printSchema = AEEP.encodePretty . AE.toJSON

printJson :: AE.Value -> BSL.ByteString
printJson = AEEP.encodePretty

printJsonToString :: AE.Value -> GHC.Base.String
printJsonToString = BSC.unpack . BSL.toStrict . printJson

printSchemaToString :: D4.Schema -> GHC.Base.String
printSchemaToString = BSC.unpack . BSL.toStrict . printSchema


parseJson :: Text -> AE.Value
parseJson json =
  (fromMaybe (panic $ "Could not parse JSON " <> json) .
   AE.decode . BSL.fromStrict . TE.encodeUtf8)
    json

validatesAll :: D4.Schema -> [AE.Value] -> Bool
validatesAll schema jsons = and $ validator <$> jsons
  where validatableSchema = D4.SchemaWithURI schema Nothing
        possibleValidator = D4.checkSchema (D4.URISchemaMap HM.empty) validatableSchema
        validator :: AE.Value -> Bool
        validator = either (\err value -> False) (null .) possibleValidator

shouldValidate :: D4.Schema -> [AE.Value] -> IO ()
shouldValidate schema jsons = do
  let validatableSchema = D4.SchemaWithURI schema Nothing
  results <- sequence $ fmap (D4.fetchFilesystemAndValidate validatableSchema) jsons
  results `shouldBe` replicate (length jsons) (Right ())

shouldNotValidateTexts :: Text -> [Text] -> IO ()
shouldNotValidateTexts expectedSchema jsonTexts = do
  let jsonInstances = fmap parseJson jsonTexts
  let schema = parseSchema expectedSchema

  schema `shouldNotValidate` jsonInstances

shouldNotValidate :: D4.Schema -> [AE.Value] -> IO ()
shouldNotValidate schema jsons = do
  let validatableSchema = D4.SchemaWithURI schema Nothing
  results <- sequence $ fmap (D4.fetchFilesystemAndValidate validatableSchema) jsons

  all isLeft results `shouldBe` True


testJsonToSchema :: Text -> Text -> IO ()
testJsonToSchema jsonText = testJsonsToSchema [jsonText]

testJsonsToSchemaWithConfig :: SchemaGenerationConfig -> [Text] -> Text -> IO ()
testJsonsToSchemaWithConfig c jsonTexts expectedSchema = do
  let jsonInstances = fmap parseJson jsonTexts
  let computedSchema = fromMaybe
                   (panic "Could not unify multiple schemas")
                   (jsonsToSchemaWithConfig c jsonInstances)

  -- Check schema is as expected
  computedSchema `shouldBe` parseSchema expectedSchema

  -- Check all provided instances validate against the computed schema
  let validatableSchema = D4.SchemaWithURI computedSchema Nothing
  results <- sequence $ fmap (D4.fetchFilesystemAndValidate validatableSchema) jsonInstances

  results `shouldBe` replicate (length jsonInstances) (Right ())

testJsonsToSchema :: [Text] -> Text -> IO ()
testJsonsToSchema = testJsonsToSchemaWithConfig defaultSchemaGenerationConfig

testJsonsToSchemaPrettyWithConfig :: SchemaGenerationConfig -> [Text] -> Text -> IO ()
testJsonsToSchemaPrettyWithConfig c jsonTexts expectedSchema =
  maybe
    (panic "Could not unify multiple schemas")
    printSchema
    (jsonsToSchemaWithConfig c $ fmap parseJson jsonTexts) `shouldBe`
  printSchema (parseSchema expectedSchema)

testJsonsToSchemaPretty :: [Text] -> Text -> IO ()
testJsonsToSchemaPretty = testJsonsToSchemaPrettyWithConfig defaultSchemaGenerationConfig

testUnifySchemaTexts :: Text -> Text -> Text -> IO ()
testUnifySchemaTexts s1 s2 expected = testUnifySchemas (parseSchema s1) (parseSchema s2) (parseSchema expected)

testUnifySchemas :: D4.Schema -> D4.Schema -> D4.Schema -> IO ()
testUnifySchemas s1 s2 expected = unifySchemas s1 s2 `shouldBe` expected
