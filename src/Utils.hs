module Utils
  (altMaybe, maxMaybe, minMaybe, andMaybe,
  computeMaximumConstraints, computeMinimumConstraints)
  where

import Protolude

import qualified Safe as S
import qualified Data.Scientific as DS


-- ordMaybe runs the binary function f on its arguments
-- and returns the Just maximum of extant values or nothing if
-- both are nothing
altMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
altMaybe f a b = f <$> a <*> b <|> a <|> b

-- Returns the maximum of the Just values or Nothing if there are no Justs
maxMaybe :: (Ord a) => [Maybe a] -> Maybe a
maxMaybe = S.maximumMay . catMaybes

minMaybe :: (Ord a) => [Maybe a] -> Maybe a
minMaybe = S.minimumMay . catMaybes


-- Make certain functions return Nothing when handed an empty list instead
-- of carrying on with their current behavior
emptyFold :: (Foldable t) => (t a -> a) -> t a -> Maybe a
emptyFold f tma
    | null tma = Nothing
    | otherwise = Just $ f tma

-- Returns the and of the Just values or Nothing if there are no Justs
andMaybe :: [Maybe Bool] -> Maybe Bool
andMaybe = emptyFold and . catMaybes

orMaybe :: [Maybe Bool] -> Maybe Bool
orMaybe = emptyFold or . catMaybes

-- TODO: this function is not particularly general because of the
-- random Down in the middle
computeConstraints :: (Ord a, Ord b) => (((a, b) -> (a, b) -> Ordering) -> [(c, d)] -> e) -> [c] -> [d] -> e
computeConstraints f bs cs = f zipComparer (zip bs cs)
    where
        zipComparer (m1, em1) (m2, em2) =
            if m1 == m2 then compare (Down em1) (Down em2) else compare m1 m2

computeMaximumConstraints :: [Maybe DS.Scientific] -> [Maybe Bool] -> (Maybe DS.Scientific, Maybe Bool)
computeMaximumConstraints = Utils.computeConstraints maximumBy

computeMinimumConstraints :: [Maybe DS.Scientific] -> [Maybe Bool] -> (Maybe DS.Scientific, Maybe Bool)
computeMinimumConstraints = Utils.computeConstraints minimumBy
