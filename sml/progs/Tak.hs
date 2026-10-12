{-# LANGUAGE Strict #-}
-- tak.sml (3cps test/programs/llvm-sml): the Takeuchi function.
module Main (main) where

import Repeat

tak :: Int -> Int -> Int -> Int
tak x y z
  | y < x     = tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
  | otherwise = z

main :: IO ()
main = do
  n <- iterations 200
  putStrLn (if repeatAll n (\i -> tak (vary i 14 0) 11 0 == 11) then "pass" else "fail")
