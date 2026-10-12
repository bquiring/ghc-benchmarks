-- environment-sharing.sml (3cps test/programs/higher-order): towers of
-- nested and sibling closures over overlapping sets of free variables, and
-- a closure that must not keep an unused list alive.  Literals get an
-- opaque zero k added each round (Repeat.vary).
module Main (main) where

import Repeat

tower :: Int -> Int -> Int -> Int -> Int -> (Int -> Int, Int -> Int, Int -> Int, Int -> Int)
tower v1 v2 v3 v4 v5 =
  ( \x -> v1 + v2 + x
  , \x -> v3 + v4 + v5 + x
  , \x -> v1 + v2 + v3 + v4 + v5 + x
  , \x -> v1 + x )

siblings :: Int -> Int -> Int -> (Int -> Int, Int -> Int, Int -> Int, Int -> Int, Int -> Int)
siblings a b c = (\x -> a + b + x, \x -> a + c + x, \x -> b + c + x, \x -> a + x, \x -> b + x)

spaceSafe :: Int -> Int -> Int
spaceSafe a = let _big = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10 :: Int]
                  small = a + 7
              in \x -> small + x

foldlH :: (a -> b -> b) -> b -> [a] -> b
foldlH _ acc [] = acc
foldlH f acc (x : xr) = foldlH f (f x acc) xr

total :: Int -> Int
total k =
  let (f12, f345, fall, f1only) = tower (1 + k) (2 + k) (3 + k) (4 + k) (5 + k)
      r1 = f12 (10 + k)
      r2 = f345 (10 + k)
      r3 = fall (10 + k)
      r4 = f1only (10 + k)
      (s1, s2, s3, s4, s5) = siblings (3 + k) (4 + k) (5 + k)
      r5 = s1 (1 + k)
      r6 = s2 (1 + k)
      r7 = s3 (1 + k)
      r8 = s4 (1 + k)
      r9 = s5 (1 + k)
      sf = spaceSafe (3 + k)
      r10 = sf (2 + k)
      cls = [f12, f345, fall, f1only, s1, s2, s3, s4, s5, sf]
      r11 = foldlH (\f acc -> acc + f (0 + k)) 0 cls
  in r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 + r10 + r11

main :: IO ()
main = do
  n <- iterations 8064612
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 191 then "pass" else "fail")
