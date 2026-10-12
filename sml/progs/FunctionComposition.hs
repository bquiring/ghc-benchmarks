-- function-composition.sml (3cps test/programs/higher-order): static
-- composed pipelines (top-level, as in the SML), a pipeline folded from a
-- list, and iterate.  Literals get an opaque zero k added each round.
module Main (main) where

import Repeat

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \x -> f (g x)

add1, double, negate', triple, addFive :: Int -> Int
add1 x = x + 1
double x = x * 2
negate' x = 0 - x
triple x = x * 3
addFive x = x + 5

pipeline, pipeline2, dynPipeline :: Int -> Int
pipeline = compose negate' (compose double add1)
pipeline2 = compose addFive (compose negate' (compose double (compose triple add1)))
dynPipeline = composeAll [add1, double, triple]

composeAll :: [Int -> Int] -> Int -> Int
composeAll [] = \x -> x
composeAll (f : rest) = compose f (composeAll rest)

iterate' :: Int -> (a -> a) -> a -> a
iterate' n f x = if n == 0 then x else iterate' (n - 1) f (f x)

total :: Int -> Int
total k =
  let r1 = pipeline (3 + k)
      r2 = pipeline2 (2 + k)
      r3 = dynPipeline (2 + k)
      r4 = iterate' (3 + k) add1 (0 + k)
      r5 = iterate' (2 + k) double (1 + k)
  in r1 + r2 + r3 + r4 + r5

main :: IO ()
main = do
  n <- iterations 46771520
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == -1 then "pass" else "fail")
