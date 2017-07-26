module JSONSchema.QuickCheckTests where

import           Protolude

import qualified Data.Aeson                        as AE
import qualified Data.ByteString.Char8             as BSC
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.HashMap.Lazy                 as HM
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Vector                       as V
import           JSONSchema.Draft4
import           JSONSchema.Validator.Utils

import qualified JSONSchema.Draft4                 as D4
import           JSONSchema.SchemaConverter        as JSSC
import           JSONSchema.SchemaGenerationConfig
import           JSONSchema.Unifiers               as JU

import           Data.Generics.Uniplate.Data
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property          as TQP
import           TestUtils

import qualified GHC.Base

tupleTypedArrayConfig = defaultSchemaGenerationConfig {
  typeArraysAsTuples = True
}

sealedObjectPropertiesConfig = defaultSchemaGenerationConfig {
  sealObjectProperties = True
}

instance Arbitrary AE.Value where
  arbitrary = sized sizedArbitraryValue
  shrink = concatMap simpleShrink . universe

sizedArbitraryValue :: Int -> Gen AE.Value
sizedArbitraryValue n
  | n <= 0 = oneof [
        pure AE.Null,
        AE.Bool <$> arbitrary,
        AE.Number <$> arbitrary,
        AE.String <$> arbitrary
    ]
  | otherwise = resize (div n 2) $ oneof [
        pure AE.Null,
        AE.Bool <$> arbitrary,
        AE.Number <$> arbitrary,
        AE.String <$> arbitrary,
        AE.Array <$> arbitrary,
        AE.Object <$> arbitrary
    ]

simpleShrink           :: AE.Value -> [AE.Value]
simpleShrink (AE.Array a) = map (AE.Array . V.fromList) $ shrink $ V.toList a
simpleShrink (AE.Object o) = map (AE.Object . HM.fromList) $ shrink $ HM.toList o
simpleShrink _          = [] -- Nothing for simple objects

sizedJsonProp :: Int -> (AE.Value -> Property) -> Property
sizedJsonProp size = forAllShrink jsonGen (concatMap simpleShrink . universe)
  where
    jsonGen :: Gen AE.Value
    jsonGen = resize size arbitrary

-- N.B. it is in general not true that the empty schema acts as the identity
-- value with regards to unifySchema because the empty schema represents a
-- more relaxed value exclusiveMinimum, exclusiveMaximum, and required, which we
-- must respect. To get around this, we create a newtype RestrictedSchema
-- which has a different instance of Arbitrary which always returns Nothing for
-- the more relaxed values, thus avoiding this issue.
newtype RestrictedSchema = RestrictedSchema { getSchema :: Schema } deriving (Show)

instance Arbitrary RestrictedSchema where
    arbitrary = sized f
      where
        maybeGen :: Gen a -> Gen (Maybe a)
        maybeGen a = oneof [pure Nothing, Just <$> a]

        maybeRecurse :: Int -> Gen a -> Gen (Maybe a)
        maybeRecurse n a
            | n < 1     = pure Nothing
            | otherwise = maybeGen $ resize (n `div` 10) a

        f :: Int -> Gen RestrictedSchema
        f n = do
            a  <- maybeGen arbitraryText
            b  <- maybeGen arbitraryText
            c  <- maybeGen arbitraryText

            d  <- pure Nothing
            e  <- pure mempty

            f' <- maybeGen arbitraryPositiveScientific
            g  <- maybeGen arbitraryScientific
            h  <- pure Nothing
            i  <- maybeGen arbitraryScientific
            j  <- pure Nothing

            k  <- maybeGen (getPositive <$> arbitrary)
            l  <- maybeGen (getPositive <$> arbitrary)
            m  <- maybeGen arbitraryText

            n' <- maybeGen (getPositive <$> arbitrary)
            o  <- maybeGen (getPositive <$> arbitrary)
            p  <- arbitrary
            q  <- maybeRecurse n arbitrary
            r  <- maybeRecurse n arbitrary

            s  <- maybeGen (getPositive <$> arbitrary)
            t  <- maybeGen (getPositive <$> arbitrary)
            u  <- pure Nothing
            v  <- maybeRecurse n arbitraryHashMap
            w  <- maybeRecurse n arbitraryHashMap
            x  <- maybeRecurse n arbitraryHashMap
            y  <- maybeRecurse n arbitrary

            z  <- maybeRecurse n ( fmap _unArbitraryValue . _unNonEmpty'
                               <$> arbitrary)
            a2 <- arbitrary
            b2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            c2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            d2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
            e2 <- maybeRecurse n arbitrary
            pure $ RestrictedSchema Schema
                { _schemaVersion              = a
                , _schemaId                   = b
                , _schemaRef                  = c
                , _schemaDefinitions          = d
                , _schemaOther                = e

                , _schemaMultipleOf           = f'
                , _schemaMaximum              = g
                , _schemaExclusiveMaximum     = h
                , _schemaMinimum              = i
                , _schemaExclusiveMinimum     = j

                , _schemaMaxLength            = k
                , _schemaMinLength            = l
                , _schemaPattern              = m

                , _schemaMaxItems             = n'
                , _schemaMinItems             = o
                , _schemaUniqueItems          = p
                , _schemaItems                = q
                , _schemaAdditionalItems      = r

                , _schemaMaxProperties        = s
                , _schemaMinProperties        = t
                , _schemaRequired             = u
                , _schemaDependencies         = v
                , _schemaProperties           = w
                , _schemaPatternProperties    = x
                , _schemaAdditionalProperties = y

                , _schemaEnum                 = z
                , _schemaType                 = a2
                , _schemaAllOf                = b2
                , _schemaAnyOf                = c2
                , _schemaOneOf                = d2
                , _schemaNot                  = e2
                }


explainSchemaCounterexample :: AE.Value -> D4.Schema -> GHC.Base.String
explainSchemaCounterexample json schema =
  "The JSON " <> printJsonToString json <> " does not validate against its generated schema " <> printSchemaToString schema

testPropUnifyEmptySchemaRightIdentity :: Spec
testPropUnifyEmptySchemaRightIdentity = it "will not change a restricted schema when an empty schema is passed in on the right" $
  property prop
  where
    prop :: RestrictedSchema -> Bool
    prop rs = JU.unifySchemas (getSchema rs) emptySchema == getSchema rs

testPropUnifyEmptySchemaLeftIdentity :: Spec
testPropUnifyEmptySchemaLeftIdentity = it "will not change a restricted schema when an empty schema is passed in on the left" $
  property prop
  where
    prop :: RestrictedSchema -> Bool
    prop rs = JU.unifySchemas emptySchema (getSchema rs) == getSchema rs

testJsonToSchemaValidatesJson :: Spec
testJsonToSchemaValidatesJson = it "will generate a schema that can validate the JSON used to generate the schema" $
  sizedJsonProp 50 prop
  where
    prop :: AE.Value -> Property
    prop json = counterexample (explainSchemaCounterexample json schema) (schema `validatesAll` [json])
      where schema = JSSC.jsonToSchema json
