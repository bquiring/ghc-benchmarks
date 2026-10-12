-- y-fact.sml (3cps test/programs/llvm-sml): factorial through a "Y"
-- combinator.  The SML loops 100000 times itself.
module Main (main) where

import Repeat

y :: ((Int -> Int) -> Int -> Int) -> Int -> Int
y f x = let g x' = y f x' in f g x

factY :: (Int -> Int) -> Int -> Int
factY fact n = if n == 0 then 1 else n * fact (n - 1)

fact :: Int -> Int
fact = y factY

main :: IO ()
main = do
  n <- iterations 30000000
  let ok i = fact (vary i 11 0) + fact (vary i 10 0) == 39916800 + 3628800
  putStrLn (if repeatAll n ok then "pass" else "fail")
