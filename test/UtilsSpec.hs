{-# LANGUAGE QuasiQuotes #-}

module UtilsSpec where

import           Data.Scientific
import qualified JSONSchema.Draft4                  as D4
import           JSONSchema.Draft4.SchemaGeneration
import qualified JSONSchema.Validator.Draft4.Any    as V4A
import           JSONSchema.Draft4.Internal.Utils
import           Test.Hspec

import           NeatInterpolation
import           Protolude

import qualified Data.Aeson                         as AE
import qualified Data.Aeson.Encode.Pretty           as AEEP
import qualified Data.ByteString.Lazy               as BSL
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE

spec :: Spec
spec = do
  describe "Maximum Constraint Tests" maximumConstraintTests
  describe "Minimum Constraint Tests" minimumConstraintTests

maximumConstraintTests :: Spec
maximumConstraintTests = do
  it "will compute a maximum from some numbers" $
    computeMaximumConstraints
      [ Just $ scientific 20 1
      , Just $ scientific 30 1
      , Nothing
      , Nothing
      , Just $ scientific 25 1
      ]
      [Just True, Just False, Nothing, Just True, Just True] `shouldBe`
    (Just $ scientific 30 1, Just False)
  it
    "will compute a maximum from the existence of an exclusive maximum when there is a tie" $
    computeMaximumConstraints
      [ Just $ scientific 59 1
      , Just $ scientific 52 1
      , Nothing
      , Just $ scientific 59 1
      , Nothing
      ]
      [Just False, Just True, Just True, Just True, Just True] `shouldBe`
    (Just $ scientific 59 1, Just False)
  it "will be able to return a sensible value when nothing is defined" $
    computeMaximumConstraints
      [Nothing, Nothing, Nothing]
      [Nothing, Nothing, Nothing] `shouldBe`
    (Nothing, Nothing)

minimumConstraintTests :: Spec
minimumConstraintTests = do
  it "will compute a minimum from some numbers" $
    computeMinimumConstraints
      [ Just $ scientific 20 1
      , Just $ scientific 30 1
      , Nothing
      , Nothing
      , Just $ scientific 25 1
      ]
      [Just True, Just False, Nothing, Just True, Just True] `shouldBe`
    (Just $ scientific 20 1, Just True)
  it
    "will compute a minimum from the existence of an exclusive minimum when there is a tie" $
    computeMinimumConstraints
      [ Just $ scientific 59 1
      , Nothing
      , Just $ scientific 12 1
      , Just $ scientific 14 1
      , Just $ scientific 12 1
      , Just $ scientific 59 1
      , Nothing
      ]
      [ Just False
      , Just True
      , Just True
      , Just True
      , Just False
      , Just False
      , Just True
      ] `shouldBe`
    (Just $ scientific 12 1, Just False)
  it "will be able to return a sensible value when nothing is defined" $
    computeMinimumConstraints
      [Nothing, Nothing, Nothing]
      [Nothing, Nothing, Nothing] `shouldBe`
    (Nothing, Nothing)
