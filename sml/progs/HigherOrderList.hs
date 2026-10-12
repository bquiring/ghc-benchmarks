-- higher-order-list.sml (3cps test/programs/higher-order): map, filter and
-- folds with function arguments, several lambdas flowing to one map, a fold
-- whose accumulator threads a list of functions.  Literals get an opaque
-- zero k added each round.  (The SML's unused evens and evens2 are kept;
-- being lazy, they are not computed.)
module Main (main) where

import Repeat

mapH :: (a -> b) -> [a] -> [b]
mapH _ [] = []
mapH f (x : xr) = f x : mapH f xr

foldlH :: (a -> b -> b) -> b -> [a] -> b
foldlH _ acc [] = acc
foldlH f acc (x : xr) = foldlH f (f x acc) xr

filterH :: (a -> Bool) -> [a] -> [a]
filterH _ [] = []
filterH p (x : xr) = if p x then x : filterH p xr else filterH p xr

sumH :: [Int] -> Int
sumH = foldlH (\x acc -> x + acc) 0

total :: Int -> Int
total k =
  let xs = [1 + k, 2 + k, 3 + k, 4 + k, 5 + k]
      squared = mapH (\x -> x * x) xs
      _evens = filterH (\x -> x - (x - x) == x && (x - 2 * (x - x)) == x) xs
      _evens2 = filterH (\x -> (x - 2 * (x - x - x + x)) == x) xs
      evens3 = filterH (\x -> x == 2 || x == 4) xs
      doubled = mapH (\x -> x * 2) xs
      negated = mapH (\x -> 0 - x) xs
      inced = mapH (\x -> x + 1) xs
      xss = [[1 + k, 2 + k], [3 + k, 4 + k], [5 + k]]
      lifted = mapH (mapH (\x -> x + 10)) xss
      fns = [\x -> x + 1, \x -> x * 2, \x -> x + 3]
      rfns = foldlH (\g acc -> g acc) (0 + k) fns
      pipe = sumH (filterH (\x -> x > 5) (mapH (\x -> x * 3) xs))
  in sumH squared + sumH evens3 + sumH doubled + sumH negated + sumH inced
     + sumH (mapH sumH lifted) + rfns + pipe

main :: IO ()
main = do
  n <- iterations 1613788
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 208 then "pass" else "fail")
