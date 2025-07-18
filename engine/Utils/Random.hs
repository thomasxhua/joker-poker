module Utils.Random
    ( shuffle
    , takeShuffled
    ) where

import Data.Array.IO (newListArray,getElems)
import System.Random (randomRIO)
import Control.Monad (forM_)

import Utils.Array

shuffle :: [a] -> IO [a]
shuffle xs = do
    -- Fisher-Yates
    let size = length xs
    arr <- newListArray (0, size-1) xs
    forM_ [size-1,size-2..0] $ \i -> do
        j <- randomRIO(0,i)
        swap arr i j
    getElems arr

takeShuffled :: Int -> [a] -> IO [a]
takeShuffled n xs = do
    xs' <- shuffle xs
    return $ take n xs'

