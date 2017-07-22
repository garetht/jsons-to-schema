import Test.Hspec
import Utils
import Data.Scientific

import Protolude


main :: IO ()
main = hspec $ do
    describe "Constraint Tests" constraintTests

constraintTests :: Spec
constraintTests = do
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



