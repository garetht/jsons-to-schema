{-# LANGUAGE QuasiQuotes #-}

module JSONSchema.Draft4.UnifiersSpec where

import           JSONSchema.Draft4
import           NeatInterpolation
import           Protolude
import           Test.Hspec
import           TestUtils

testUnifyEmptySchemas :: Spec
testUnifyEmptySchemas =
  it "will unify two empty schemas" $
  testUnifySchemas emptySchema emptySchema emptySchema

testUnifySingleValueConstraints :: Spec
testUnifySingleValueConstraints =
  it "will unify schemas with single values" $
  let s1 =
        [text|
               {
                "version": "1.0.0"
               }
             |]
      s2 =
        [text|
              {
               "version": "2.4.6"
              }
             |]
      expected =
        [text|
            {
              "version": "1.0.0"
            }
        |]
  in testUnifySchemaTexts s1 s2 expected

testUnifyMultipleSingleValueConstraints :: Spec
testUnifyMultipleSingleValueConstraints =
  it "will unify schemas with single values" $
  let s1 =
        [text|
               {
                "version": "1.0.0",
                "id": "x-1234"
               }
             |]
      s2 =
        [text|
              {
               "version": "2.4.6",
               "id": "x-2019",
               "pattern": ".+"
              }
             |]
      expected =
        [text|
            {
              "version": "1.0.0",
              "id": "x-1234",
              "pattern": ".+"
            }
        |]
  in testUnifySchemaTexts s1 s2 expected

spec :: Spec
spec =
  describe "Basic Unification" $ do
    testUnifyEmptySchemas
    testUnifySingleValueConstraints
    testUnifyMultipleSingleValueConstraints
