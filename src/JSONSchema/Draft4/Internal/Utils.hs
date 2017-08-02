{-|
  Module      : JSONSchema.Draft4.Internal.Utils
  Description : Internal utilities.
  Copyright   : (c) Gareth Tan, 2017
  License     : MIT

  Assorted internal utilities.
-}
module JSONSchema.Draft4.Internal.Utils
  ( alt
  , andMaybe
  , computeMaximumConstraints
  , computeMinimumConstraints
  , zipWithPadding
  , listToMaybeList
  , setToMaybeSet
  , parseValue
  , printSchema
  ) where

import           Protolude

import qualified Data.Aeson               as AE
import qualified Data.Aeson.Encode.Pretty as AEEP
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Scientific          as DS
import qualified Data.Set                 as DS
import qualified Data.Text.Encoding       as TE
import qualified JSONSchema.Draft4        as D4

-- |Functions like an Applicative but keeps the 'Just' value if it exists.
alt :: Alternative f => (a -> a -> a) -> f a -> f a -> f a
alt f a b = f <$> a <*> b <|> a <|> b

-- |Make certain functions return Nothing when handed an empty list instead
-- of carrying on with their current behavior
emptyFold :: (Foldable t) => (t a -> a) -> t a -> Maybe a
emptyFold f tma
  | null tma = Nothing
  | otherwise = Just $ f tma

-- |Returns the and of the Just values or Nothing if there are no Justs
andMaybe :: [Maybe Bool] -> Maybe Bool
andMaybe = emptyFold and . catMaybes

{-| The unification function for the integer constraints @maximum@ and @exclusiveMaximum@.
    When unifying schemas the @exclusiveMaximum@ may somestimes need to be modified
    based on the @maximum.
-}
computeMaximumConstraints :: [Maybe DS.Scientific] -> [Maybe Bool] -> (Maybe DS.Scientific, Maybe Bool)
computeMaximumConstraints maxes emaxes = maximumBy zipComparer (zip maxes emaxes)
    -- We use Down to reverse the compare order so that False > True
    -- We need False > True because if two schemas have the same maximum
    -- but one excludes the maximum then we want the schema to not exclude the
    -- maximum (i.e. it is more inclusive)
  where
    zipComparer (m1, em1) (m2, em2) =
      if m1 == m2
        then if and $ isNothing <$> [m1, m2]
               then compare em1 em2
               else compare (Down em1) (Down em2)
        else compare m1 m2

-- Usually the ordering goes like this: Nothing < Just 20 < Just 30, and so the
-- minimum is Nothing. But we want the ordering to be Just 20 < Just 30 < Nothing
-- (Down would provide the ordering Just 30 < Just 20 < Nothing), so we provide
-- a custom comparison function
{-| The unification function for the integer constraints @minimum@ and @exclusiveMinimum@.
    When unifying schemas the @exclusiveMinimum@ may somestimes need to be modified
    based on the @minimum.
-}
computeMinimumConstraints :: [Maybe DS.Scientific] -> [Maybe Bool] -> (Maybe DS.Scientific, Maybe Bool)
computeMinimumConstraints mins emins = minimumBy zipComparer (zip mins emins)
  where
    justComparer :: (Ord a) => Maybe a -> Maybe a -> Ordering
    justComparer (Just x) (Just y) = compare x y
    justComparer (Just _) Nothing  = LT
    justComparer Nothing (Just _)  = GT
    justComparer Nothing Nothing   = EQ
    zipComparer (m1, em1) (m2, em2) =
      if m1 == m2
        then if and $ isNothing <$> [m1, m2]
               then compare (Down em1) (Down em2)
               else compare em1 em2
        else justComparer m1 m2

-- This function is from StackOverflow
-- | Zips a list but uses the default value if one list is longer than the other.
zipWithPadding :: a -> b -> [a] -> [b] -> [(a, b)]
zipWithPadding a b (x:xs) (y:ys) = (x, y) : zipWithPadding a b xs ys
zipWithPadding a _ [] ys         = zip (repeat a) ys
zipWithPadding _ b xs []         = zip xs (repeat b)

{-| Similar to 'listFromMaybe', but returns the entire list as a
    'Just' if it is not empty instead of only the first item. -}
listToMaybeList :: [a] -> Maybe [a]
listToMaybeList [] = Nothing
listToMaybeList xs = Just xs

{-| Returns the entire set as a 'Just' if it is not empty
    instead of only one item. -}
setToMaybeSet :: DS.Set a -> Maybe (DS.Set a)
setToMaybeSet s
  | DS.null s = Nothing
  | otherwise = Just s

{-| Parses a bytestring to a value. -}
parseValue :: BS.ByteString -> AE.Value
parseValue s = fromMaybe (panic $ "Failed to parse JSON document " <> TE.decodeUtf8 s) . AE.decode . BSL.fromStrict $ s

{-| Converts a schema to text. -}
printSchema :: D4.Schema -> Text
printSchema = TE.decodeUtf8 . BSL.toStrict . AEEP.encodePretty . AE.toJSON
