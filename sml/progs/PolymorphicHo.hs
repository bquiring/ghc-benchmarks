-- polymorphic-ho.sml (3cps test/programs/higher-order): polymorphic
-- higher-order functions used at several types.  Literals get an opaque
-- zero k added each round (Repeat.vary).
module Main (main) where

import Repeat

idH :: a -> a
idH x = x

constH :: a -> b -> a
constH x _ = x

apply :: (a -> b) -> a -> b
apply f x = f x

flipH :: (b -> a -> c) -> a -> b -> c
flipH f x y = f y x

twice :: (a -> a) -> a -> a
twice f x = f (f x)

mapOption :: (a -> b) -> Maybe a -> Maybe b
mapOption _ Nothing = Nothing
mapOption f (Just x) = Just (f x)

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

revAppend :: [a] -> [a] -> [a]
revAppend [] ys = ys
revAppend (x : xr) ys = revAppend xr (x : ys)

rev :: [a] -> [a]
rev xs = revAppend xs []

len :: [a] -> Int
len [] = 0
len (_ : rest) = 1 + len rest

foldlH :: (a -> b -> b) -> b -> [a] -> b
foldlH _ acc [] = acc
foldlH f acc (x : xr) = foldlH f (f x acc) xr

sumH :: [Int] -> Int
sumH = foldlH (\x a -> x + a) 0

mapH :: (a -> b) -> [a] -> [b]
mapH _ [] = []
mapH f (x : xr) = f x : mapH f xr

total :: Int -> Int
total k =
  let r1 = apply (\x -> x + 1) (5 + k)
      r2 = len (apply rev [1 + k, 2 + k, 3 + k])
      r3 = apply (\b -> if b then 1 else 0) (k == 0)
      r4 = twice (\x -> x + 1) (3 + k)
      r5 = len (twice rev [1 + k, 2 + k, 3 + k])
      r6 = case mapOption (\x -> x * 2) (Just (5 + k)) of { Just x -> x; Nothing -> 0 }
      r7 = case mapOption (\x -> x + 1) (if k == 0 then Nothing else Just k) of { Just x -> x; Nothing -> 0 }
      (r8a, r8b) = mapPair (\x -> x * 3) (\x -> x + 1) (4 + k, 5 + k)
      r8 = r8a + r8b
      r9 = constH (42 + k) (999 + k)
      r10 = flipH (\x y -> x - y) (1 + k) (10 + k)
      fns = [idH, twice (\x -> x + 1), apply (\x -> x + 1)]
      r11 = sumH (mapH (\f -> f (5 + k)) fns)
  in r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 + r10 + r11

main :: IO ()
main = do
  n <- iterations 6907535
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 115 then "pass" else "fail")
