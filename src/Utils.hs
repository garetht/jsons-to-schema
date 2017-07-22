module Utils
  (maxMaybe, minMaybe)
  where

import Protolude

import qualified Safe as S

-- ordMaybe runs the binary function f on its arguments
-- and returns the Just maximum of extant values or nothing if
-- both are nothing
ordMaybe :: (Ord a) => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
ordMaybe f a b = f <$> a <*> b <|>a <|>b

maxMaybe :: (Ord a) => [Maybe a] -> Maybe a
maxMaybe = S.maximumMay . catMaybes

minMaybe :: (Ord a) => [Maybe a] -> Maybe a
minMaybe = S.minimumMay . catMaybes
