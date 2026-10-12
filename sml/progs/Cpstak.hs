-- cpstak.sml (3cps test/programs/llvm-sml), from the Larceny benchmarks:
-- the Takeuchi function in continuation-passing style.
module Main (main) where

import Repeat

cpstak :: Int -> Int -> Int -> Int
cpstak x0 y0 z0 = tak x0 y0 z0 (\a -> a)
  where
    tak :: Int -> Int -> Int -> (Int -> Int) -> Int
    tak x y z k
      | y >= x    = k z
      | otherwise = tak (x - 1) y z (\v1 ->
                      tak (y - 1) z x (\v2 ->
                        tak (z - 1) x y (\v3 ->
                          tak v1 v2 v3 k)))

main :: IO ()
main = do
  n <- iterations 100
  putStrLn (if repeatAll n (\i -> cpstak (vary i 14 0) 11 0 == 11) then "pass" else "fail")
