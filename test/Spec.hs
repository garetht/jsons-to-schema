{-# LANGUAGE QuasiQuotes #-}

import           Data.Scientific
import qualified JSONSchema.Draft4               as D4
import           JSONSchema.SchemaConverter
import qualified JSONSchema.Validator.Draft4.Any as V4A
import           Test.Hspec
import           Utils

import           NeatInterpolation
import           Protolude

import qualified Data.Aeson                      as AE
import qualified Data.Aeson.Encode.Pretty        as AEEP
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE

import JSONSchema.SchemaConverter.Tests

main :: IO ()
main = hspec $ do
    describe "Maximum Constraint Tests" maximumConstraintTests
    describe "Minimum Constraint Tests" minimumConstraintTests
--     describe "Schema Unifier Tests" schemaUnifierTests
    describe "Simple Extenrnal Test" simpleExternalTest

parseSchema :: Text -> D4.Schema
parseSchema = fromMaybe (panic "Failed to parse schema") . AE.decode . BSL.fromStrict . TE.encodeUtf8

printSchema :: D4.Schema -> BSL.ByteString
printSchema = AEEP.encodePretty . AE.toJSON

objectSchema1 =
   [text|
{
  "type": "object",
  "required": ["name", "team"],
  "properties": {
    "name": {
      "type": "number",
      "maximum": 202
    },
    "otherProp": {
      "type": "string"
    },
    "team": {
      "type": "number"
    }
  },
  "maximum": 20
}
   |]

objectSchema2 =
   [text|
{
  "type": "object",
  "required": ["alternative", "team"],
  "properties": {
    "name": {
      "type": "string"
    },
    "alternative": {
        "type": "boolean"
    },
    "team": {
        "type": "number"
    }
  },
  "maximum": 40
}
   |]

objectSchema3 =
   [text|
{
  "type": "number"
}
   |]

objectSchema4 =
   [text|
{
  "type": "string"
}
   |]


schemaUnifierTests :: Spec
schemaUnifierTests =
    it "will be able to union two object schemas" $ do
        let schema1 = parseSchema objectSchema1
        let schema2 = parseSchema objectSchema2
        let schema3 = parseSchema objectSchema3
        let schema4 = parseSchema objectSchema4
--         schema1 `shouldBe` D4.emptySchema
        fmap printSchema (foldr1May unifySchemas [schema1, schema2, schema3, schema4]) `shouldBe` Just ""



maximumConstraintTests :: Spec
maximumConstraintTests = do
    it "will compute a maximum from some numbers" $
        computeMaximumConstraints
            [ Just $ scientific 20 1
            , Just $ scientific 30 1
            , Nothing
            , Nothing
            , Just $ scientific 25 1]
            [ Just True
            , Just False
            , Nothing
            , Just True
            , Just True
            ] `shouldBe` (Just $ scientific 30 1, Just False)

    it "will compute a maximum from the existence of an exclusive maximum when there is a tie" $
        computeMaximumConstraints
            [ Just $ scientific 59 1
            , Just $ scientific 52 1
            , Nothing
            , Just $ scientific 59 1
            , Nothing]
            [ Just False
            , Just True
            , Just True
            , Just True
            , Just True] `shouldBe` (Just $ scientific 59 1, Just False)

    it "will be able to return a sensible value when nothing is defined" $
        computeMaximumConstraints
            [ Nothing
            , Nothing
            , Nothing]
            [ Nothing
            , Nothing
            , Nothing] `shouldBe` (Nothing, Nothing)


minimumConstraintTests :: Spec
minimumConstraintTests = do
    it "will compute a minimum from some numbers" $
        computeMinimumConstraints
            [ Just $ scientific 20 1
            , Just $ scientific 30 1
            , Nothing
            , Nothing
            , Just $ scientific 25 1]
            [ Just True
            , Just False
            , Nothing
            , Just True
            , Just True
            ] `shouldBe` (Just $ scientific 20 1, Just True)

    it "will compute a minimum from the existence of an exclusive minimum when there is a tie" $
        computeMinimumConstraints
            [ Just $ scientific 59 1
            , Nothing
            , Just $ scientific 12 1
            , Just $ scientific 14 1
            , Just $ scientific 12 1
            , Just $ scientific 59 1
            , Nothing]
            [ Just False
            , Just True
            , Just True
            , Just True
            , Just False
            , Just False
            , Just True] `shouldBe` (Just $ scientific 12 1, Just False)

    it "will be able to return a sensible value when nothing is defined" $
        computeMinimumConstraints
            [ Nothing
            , Nothing
            , Nothing]
            [ Nothing
            , Nothing
            , Nothing] `shouldBe` (Nothing, Nothing)


-- Test: unifying with the empty schema is the identity function
-- in either order
