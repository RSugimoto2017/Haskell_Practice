module Lib
  ( someFunc,
    sumsquare,
    innerProduct,
    exponential,
    exponential2,
    merge,
    mergeSort,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sumsquare :: Integer -> Integer
sumsquare a
  | a == 0 = 0
  | otherwise =
    uncurry
      (*)
      (last [(x, y) | x <- [1 .. a], y <- [1 .. a], x == y])
      + sumsquare (a -1)

innerProduct :: Num a => [a] -> [a] -> a
innerProduct xs ys
  | null xs = 0
  | null ys = 0
  | otherwise = head xs * head ys + innerProduct (tail xs) (tail ys)

exponential :: Num a => a -> Int -> a
exponential a b
  | b == 0 = 1
  | otherwise = a * exponential a (b -1)

exponential2 :: Num a => a -> Int -> a
exponential2 a b
  | b == 0 = 1
  | even b = exponential2 (a ^ 2) (b `div` 2)
  | otherwise = a * exponential2 (a ^ 2) ((b -1) `div` 2)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort a) (mergeSort b)
  where
    a = take h xs
    b = drop h xs
    h = length xs `div` 2