module Utils.List
    ( allEqual
    , sortedPartitionOn
    , sortedPartition
    , subset
    , findFirstNotIn
    , findNextInCycleOn
    , findNextInCycle
    ) where

import Data.Function
import Data.List
--import Data.List (sortOn)

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual [_]    = True
allEqual (x:xs) = all (== x) xs

sortedPartitionOn :: (Eq a, Ord b) => (a -> b) -> [a] -> [[a]]
sortedPartitionOn f = groupBy ((==) `on` f) . sortOn f

sortedPartition :: Ord a => [a] -> [[a]]
sortedPartition = sortedPartitionOn id

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

findFirstNotIn :: Eq a => [a] -> [a] -> Maybe a
findFirstNotIn [] ys     = Nothing
findFirstNotIn (x:xs) ys = if x `elem` ys then findFirstNotIn xs ys else Just x

findNextInCycleOn :: (Eq b) => (a -> b) -> [a] -> b -> Maybe a
findNextInCycleOn f ys x =
    case dropWhile (\a -> f a /= x) (cycle ys) of
      (_:y:_) -> Just y
      _       -> Nothing

findNextInCycle :: Eq a => [a] -> a -> Maybe a
findNextInCycle = findNextInCycleOn id

