module Utils
  (altMaybe, andMaybe,
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
computeMaximumConstraints maxes emaxes = maximumBy zipComparer (zip maxes emaxes)
    -- We use Down to reverse the compare order so that False > True
    -- We need False > True because if two schemas have the same maximum
    -- but one excludes the maximum then we want the schema to not exclude the
    -- maximum (i.e. it is more inclusive)
    where
        zipComparer (m1, em1) (m2, em2) =
            if m1 == m2 then compare (Down em1) (Down em2) else compare m1 m2

-- Usually the ordering goes like this: Nothing < Just 20 < Just 30, and so the
-- minimum is Nothing. But we want the ordering to be Just 20 < Just 30 < Nothing
-- (Down would provide the ordering Just 30 < Just 20 < Nothing), so we provide
-- a custom comparison function
computeMinimumConstraints :: [Maybe DS.Scientific] -> [Maybe Bool] -> (Maybe DS.Scientific, Maybe Bool)
computeMinimumConstraints mins emins = minimumBy zipComparer (zip mins emins)
    where
        justComparer :: (Ord a) => Maybe a -> Maybe a -> Ordering
        justComparer (Just x) (Just y) = compare x y
        justComparer (Just _) Nothing = LT
        justComparer Nothing (Just _) = GT
        justComparer Nothing Nothing = EQ
        zipComparer (m1, em1) (m2, em2) =
            -- We don't need Down here because the comparison is already taking the minimum
            if m1 == m2 then compare em1 em2 else justComparer m1 m2

