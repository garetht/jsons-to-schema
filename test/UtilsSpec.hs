module UtilsSpec where

import           Data.Scientific
import           JSONSchema.Draft4.Internal.Utils
import           Test.Hspec

import           Protolude

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

  it "will not be clobbered by a nothing" $
    computeMaximumConstraints
      [ Just $ scientific 20 1
      , Nothing
      ]
      [Just True, Just False] `shouldBe`
    (Just $ scientific 20 1, Just True)

  it "will not be clobbered by False when both are nothing " $
    computeMaximumConstraints
      [   Nothing
        , Nothing
      ]
      [Just False, Just True] `shouldBe`
    (Nothing, Just True)

  it "will not be clobbered by Nothing when both are nothing " $
    computeMaximumConstraints
      [   Nothing
        , Nothing
      ]
      [Nothing, Just True] `shouldBe`
    (Nothing, Just True)

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

  it "will not be clobbered by a nothing" $
    computeMinimumConstraints
      [ Just $ scientific 20 1
      , Nothing
      ]
      [Just True, Just False] `shouldBe`
    (Just $ scientific 20 1, Just True)

  it "will not be clobbered by False when both are nothing " $
    computeMinimumConstraints
      [   Nothing
        , Nothing
      ]
      [Just False, Just True] `shouldBe`
    (Nothing, Just True)

  it "will not be clobbered by Nothing when both are nothing " $
    computeMinimumConstraints
      [   Nothing
        , Nothing
      ]
      [Nothing, Just True] `shouldBe`
    (Nothing, Just True)

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
