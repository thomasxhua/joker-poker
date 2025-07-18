module Utils.List
    ( allEqual
    , sortedPartition
    , subset
    ) where

import Data.List (sort)

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual [_]    = True
allEqual (x:xs) = all (== x) xs

sortedPartition :: Ord a => [a] -> [[a]]
sortedPartition = (flip partitionSorted []) . sort
    where
        partitionSorted :: Ord a => [a] -> [a] -> [[a]]
        partitionSorted [] acc  = [acc]
        partitionSorted [x] acc
          | (acc == [])     = [[x]]
          | (head acc == x) = [(x:acc)]
          | otherwise       = [acc,[x]]
        partitionSorted (x:xs) acc
          | (acc == [])     = partitionSorted xs [x]
          | (head acc == x) = partitionSorted xs (x:acc)
          | otherwise       = acc:(partitionSorted xs [x])

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (flip elem ys) xs

