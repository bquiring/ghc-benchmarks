-- defunctionalization.sml (3cps test/programs/higher-order): a closed set
-- of operations reaching one indirect call site (apply).
-- In each round the literals get an opaque zero k added (Repeat.vary), so
-- nothing is floated out of the loop.
module Main (main) where

import Repeat

apply :: (Int -> Int) -> Int -> Int
apply f x = f x

addOp, mulOp, negOp, sqOp :: Int -> Int -> Int
addOp k = \x -> x + k
mulOp k = \x -> x * k
negOp _ = \x -> 0 - x
sqOp _ = \x -> x * x

foldlH :: (b -> a -> b) -> b -> [a] -> b
foldlH _ acc [] = acc
foldlH f acc (x : xr) = foldlH f (f acc x) xr

nth :: [Int -> Int] -> Int -> (Int -> Int)
nth [] _ = \x -> x
nth (f : rest) i = if i == 0 then f else nth rest (i - 1)

total :: Int -> Int
total k =
  let ops = [addOp (3 + k), mulOp (2 + k), negOp (0 + k), sqOp (0 + k), addOp (10 + k)]
      r1 = foldlH (\acc f -> apply f acc) (1 + k) ops
      r2 = apply (nth ops (1 + k)) (5 + k)
      r3 = apply (nth ops (0 + k)) (100 + k)
      r4 = (\x -> x + 7) (3 + k)
  in r1 + r2 + r3 + r4

main :: IO ()
main = do
  n <- iterations 21568176
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 197 then "pass" else "fail")
