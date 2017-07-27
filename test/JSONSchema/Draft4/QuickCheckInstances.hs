module JSONSchema.Draft4.QuickCheckInstances where

import           Protolude

import qualified Data.Aeson                               as AE
import qualified Data.HashMap.Lazy                        as HM
import qualified Data.Vector                              as V
import           JSONSchema.Draft4
import           JSONSchema.Validator.Utils

import           Data.Generics.Uniplate.Data
import           Test.QuickCheck
import           Test.QuickCheck.Instances                ()

-- N.B. it is in general not true that the empty schema acts as the identity
-- value with regards to unifySchema because the empty schema represents a
-- more relaxed value for required, which we
-- must respect. To get around this, we create a newtype RestrictedSchema
-- which has a different instance of Arbitrary which always returns Nothing for
-- required, thus avoiding this issue.
newtype RestrictedSchema = RestrictedSchema
  { getSchema :: Schema
  } deriving (Show)

instance Arbitrary RestrictedSchema where
  arbitrary = sized f
    where
      maybeGen :: Gen a -> Gen (Maybe a)
      maybeGen a = oneof [pure Nothing, Just <$> a]
      maybeRecurse :: Int -> Gen a -> Gen (Maybe a)
      maybeRecurse n a
        | n < 1 = pure Nothing
        | otherwise = maybeGen $ resize (n `div` 10) a
      f :: Int -> Gen RestrictedSchema
      f n = do
        a <- maybeGen arbitraryText
        b <- maybeGen arbitraryText
        c <- maybeGen arbitraryText
        d <- pure Nothing
        e <- pure mempty
        f' <- maybeGen arbitraryPositiveScientific
        g <- maybeGen arbitraryScientific
        h <- maybeGen arbitrary
        i <- maybeGen arbitraryScientific
        j <- maybeGen arbitrary
        k <- maybeGen (getPositive <$> arbitrary)
        l <- maybeGen (getPositive <$> arbitrary)
        m <- maybeGen arbitraryText
        n' <- maybeGen (getPositive <$> arbitrary)
        o <- maybeGen (getPositive <$> arbitrary)
        p <- arbitrary
        q <- maybeRecurse n arbitrary
        r <- maybeRecurse n arbitrary
        s <- maybeGen (getPositive <$> arbitrary)
        t <- maybeGen (getPositive <$> arbitrary)
        u <- pure Nothing
        v <- maybeRecurse n arbitraryHashMap
        w <- maybeRecurse n arbitraryHashMap
        x <- maybeRecurse n arbitraryHashMap
        y <- maybeRecurse n arbitrary
        z <-
          maybeRecurse n (fmap _unArbitraryValue . _unNonEmpty' <$> arbitrary)
        a2 <- arbitrary
        b2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
        c2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
        d2 <- maybeRecurse n (_unNonEmpty' <$> arbitrary)
        e2 <- maybeRecurse n arbitrary
        pure $
          RestrictedSchema
            Schema {
              _schemaVersion = a
            , _schemaId = b
            , _schemaRef = c
            , _schemaDefinitions = d
            , _schemaOther = e
            , _schemaMultipleOf = f'
            , _schemaMaximum = g
            , _schemaExclusiveMaximum = h
            , _schemaMinimum = i
            , _schemaExclusiveMinimum = j
            , _schemaMaxLength = k
            , _schemaMinLength = l
            , _schemaPattern = m
            , _schemaMaxItems = n'
            , _schemaMinItems = o
            , _schemaUniqueItems = p
            , _schemaItems = q
            , _schemaAdditionalItems = r
            , _schemaMaxProperties = s
            , _schemaMinProperties = t
            , _schemaRequired = u
            , _schemaDependencies = v
            , _schemaProperties = w
            , _schemaPatternProperties = x
            , _schemaAdditionalProperties = y
            , _schemaEnum = z
            , _schemaType = a2
            , _schemaAllOf = b2
            , _schemaAnyOf = c2
            , _schemaOneOf = d2
            , _schemaNot = e2
            }

-- For properties that cannot be easily unified we simply choose
-- one (e.g. "version": "1.0" and "version": "2.0"). By excluding
-- these properties, unified with `const`, we can test if a
-- unifySchema is commutative otherwise
newtype CommutativeSchema = CommutativeSchema
  { getCommutativeSchema :: Schema
  } deriving (Show)

instance Arbitrary CommutativeSchema where
  arbitrary = sized f
    where
      maybeGen :: Gen a -> Gen (Maybe a)
      maybeGen a = oneof [pure Nothing, Just <$> a]
      maybeRecurse :: Int -> Gen a -> Gen (Maybe a)
      maybeRecurse n a
        | n < 1 = pure Nothing
        | otherwise = maybeGen $ resize (n `div` 10) a
      f :: Int -> Gen CommutativeSchema
      f n = do
        a <- pure Nothing
        b <- pure Nothing
        c <- pure Nothing
        d <- pure Nothing
        e <- pure mempty
        f' <- pure Nothing
        g <- maybeGen arbitraryScientific
        h <- maybeGen arbitrary
        i <- maybeGen arbitraryScientific
        j <- maybeGen arbitrary
        k <- maybeGen (getPositive <$> arbitrary)
        l <- maybeGen (getPositive <$> arbitrary)
        m <- pure Nothing
        n' <- maybeGen (getPositive <$> arbitrary)
        o <- maybeGen (getPositive <$> arbitrary)
        p <- arbitrary
        q <- maybeRecurse n arbitrary
        r <- maybeRecurse n arbitrary
        s <- maybeGen (getPositive <$> arbitrary)
        t <- maybeGen (getPositive <$> arbitrary)
        u <- pure Nothing
        v <- pure Nothing
        w <- maybeRecurse n arbitraryHashMap
        x <- maybeRecurse n arbitraryHashMap
        y <- maybeRecurse n arbitrary
        z <- pure Nothing
        a2 <- arbitrary
        b2 <- pure Nothing
        c2 <- pure Nothing
        d2 <- pure Nothing
        e2 <- pure Nothing
        pure $
          CommutativeSchema
            Schema {
              _schemaVersion = a
            , _schemaId = b
            , _schemaRef = c
            , _schemaDefinitions = d
            , _schemaOther = e
            , _schemaMultipleOf = f'
            , _schemaMaximum = g
            , _schemaExclusiveMaximum = h
            , _schemaMinimum = i
            , _schemaExclusiveMinimum = j
            , _schemaMaxLength = k
            , _schemaMinLength = l
            , _schemaPattern = m
            , _schemaMaxItems = n'
            , _schemaMinItems = o
            , _schemaUniqueItems = p
            , _schemaItems = q
            , _schemaAdditionalItems = r
            , _schemaMaxProperties = s
            , _schemaMinProperties = t
            , _schemaRequired = u
            , _schemaDependencies = v
            , _schemaProperties = w
            , _schemaPatternProperties = x
            , _schemaAdditionalProperties = y
            , _schemaEnum = z
            , _schemaType = a2
            , _schemaAllOf = b2
            , _schemaAnyOf = c2
            , _schemaOneOf = d2
            , _schemaNot = e2
            }

-- Arbitrary instance for Value from Reddit.
instance Arbitrary AE.Value where
  arbitrary = sized sizedArbitraryValue
  -- JSON shrinker borrowed from json-autotype
  shrink = valueShrink

sizedArbitraryValue :: Int -> Gen AE.Value
sizedArbitraryValue n
  | n <= 0 =
    oneof
      [ pure AE.Null
      , AE.Bool <$> arbitrary
      , AE.Number <$> arbitrary
      , AE.String <$> arbitrary
      ]
  | otherwise =
    resize (div n 2) $
    oneof
      [ pure AE.Null
      , AE.Bool <$> arbitrary
      , AE.Number <$> arbitrary
      , AE.String <$> arbitrary
      , AE.Array <$> arbitrary
      , AE.Object <$> arbitrary
      ]

simpleShrink :: AE.Value -> [AE.Value]
simpleShrink (AE.Array a) = map (AE.Array . V.fromList) $ shrink $ V.toList a
simpleShrink (AE.Object o) =
  map (AE.Object . HM.fromList) $ shrink $ HM.toList o
simpleShrink _ = [] -- Nothing for simple objects

valueShrink :: AE.Value -> [AE.Value]
valueShrink = concatMap simpleShrink . universe

sizedJsonProp :: Int -> (AE.Value -> Property) -> Property
sizedJsonProp size = forAllShrink jsonGen valueShrink
  where
    jsonGen :: Gen AE.Value
    jsonGen = resize size arbitrary

sizedJsonsProp :: Int -> ([AE.Value] -> Property) -> Property
sizedJsonsProp size = forAllShrink jsonGen $ shrinkList valueShrink
  where
    jsonGen :: Gen [AE.Value]
    jsonGen = resize size arbitrary
