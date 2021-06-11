module Main where

import Lib (exponential, exponential2, innerProduct, merge, mergeSort, sumsquare)

main :: IO ()
main = print (mergeSort [1, 8, 3, 2, 9, 6])
