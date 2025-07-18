module Utils.Array
    ( swap
    ) where

import Data.Array.IO (IOArray,readArray,writeArray)

swap :: IOArray Int a -> Int -> Int -> IO ()
swap arr i j = do
    arrI <- readArray arr i
    arrJ <- readArray arr j
    writeArray arr i arrJ
    writeArray arr j arrI

