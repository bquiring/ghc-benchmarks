-- returned-closures.sml (3cps test/programs/higher-order): function
-- factories returning escaping closures, applied after the factory returns.
-- Literals get an opaque zero k added each round (Repeat.vary).
module Main (main) where

import Repeat

adder, scaler :: Int -> Int -> Int
adder n = \x -> x + n
scaler k = \x -> x * k

affine :: Int -> Int -> Int -> Int
affine a b = \x -> a * x + b

twice :: (a -> a) -> a -> a
twice f = \x -> f (f x)

ignoreSecond :: Int -> Int -> Int -> Int -> Int
ignoreSecond a _ _ = \x -> a + x

mapH :: (a -> b) -> [a] -> [b]
mapH _ [] = []
mapH f (x : xr) = f x : mapH f xr

foldlH :: (a -> b -> b) -> b -> [a] -> b
foldlH _ acc [] = acc
foldlH f acc (x : xr) = foldlH f (f x acc) xr

sumH :: [Int] -> Int
sumH = foldlH (\x acc -> x + acc) 0

total :: Int -> Int
total k =
  let add3 = adder (3 + k)
      scale5 = scaler (5 + k)
      aff = affine (2 + k) (1 + k)
      incInc = twice (adder (1 + k))
      dropBoth = ignoreSecond (10 + k) (99 + k) (999 + k)
      closures = [add3, scale5, aff, incInc, dropBoth]
      r1 = sumH (mapH (\f -> f (4 + k)) closures)
      inputs = [1 + k, 2 + k, 3 + k]
      applyAll fs x = sumH (mapH (\f -> f x) fs)
      r2 = sumH (mapH (applyAll closures) inputs)
  in r1 + r2

main :: IO ()
main = do
  n <- iterations 2412969
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 164 then "pass" else "fail")
