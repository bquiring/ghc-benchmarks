-- curry-uncurry.sml (3cps test/programs/higher-order): curried functions,
-- saturated and partial applications, the same function reaching known and
-- unknown call sites.  Literals get an opaque zero k added each round.
module Main (main) where

import Repeat

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c

mul4 :: Int -> Int -> Int -> Int -> Int
mul4 a b c d = a * b * c * d

add3' :: Int -> Int -> Int -> Int
add3' = \a -> \b -> \c -> a + b + c

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
  let s1 = add3 (1 + k) (2 + k) (3 + k)
      s2 = mul4 (2 + k) (3 + k) (4 + k) (5 + k)
      s3 = add3' (10 + k) (20 + k) (30 + k)
      addTen = add3 (10 + k)
      inc = add3 (1 + k) (0 + k)
      r1 = addTen (1 + k) (2 + k)
      r2 = inc (3 + k)
      partials = [add3 (1 + k) (0 + k), add3 (2 + k) (0 + k), add3 (3 + k) (0 + k)]
      results = mapH (\f -> f (10 + k)) partials
  in s1 + s2 + s3 + r1 + r2 + sumH results

main :: IO ()
main = do
  n <- iterations 23077555
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 239 then "pass" else "fail")
