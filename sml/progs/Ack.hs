{-# LANGUAGE Strict #-}
-- ack.sml (3cps test/programs/llvm-sml), from the Larceny benchmarks:
-- Ackermann's function.
module Main (main) where

import Repeat

ack :: Int -> Int -> Int
ack m n
  | m == 0    = n + 1
  | n == 0    = ack (m - 1) 1
  | otherwise = ack (m - 1) (ack m (n - 1))

main :: IO ()
main = do
  n <- iterations 4
  let ok i = let n1 = ack 3 (vary i 11 0)
                 n2 = ack 2 (vary i 5 0)
             in (n1, n2) == (16381, 13)
  putStrLn (if repeatAll n ok then "pass" else "fail")
