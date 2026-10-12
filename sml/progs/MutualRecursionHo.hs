-- mutual-recursion-ho.sml (3cps test/programs/higher-order): mutually
-- recursive functions passing continuations and folding functions.
-- Literals get an opaque zero k added each round (Repeat.vary).
module Main (main) where

import Repeat

evenK, oddK :: Int -> (Bool -> a) -> a
evenK n k = if n == 0 then k True else oddK (n - 1) k
oddK n k = if n == 0 then k False else evenK (n - 1) k

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

data Tree = LeafT Int | NodeA Tree Tree | NodeB Int Tree

travB :: Tree -> (Int -> b -> b) -> b -> b
travB t f acc = case t of
  LeafT x -> f x acc
  NodeA l r -> travB r f (travB l f acc)
  NodeB k c -> travB c f (f k acc)

evenStep, oddStep :: Int -> (Int -> a) -> a
evenStep n k = if n == 0 then k 0 else oddStep (n - 1) (\x -> k (x + 1))
oddStep n k = if n == 0 then k 0 else evenStep (n - 1) (\x -> k (x + 2))

total :: Int -> Int
total k =
  let r1 = evenK (6 + k) boolToInt
      r2 = oddK (5 + k) boolToInt
      r3 = evenK (3 + k) boolToInt
      t = NodeA (NodeB (5 + k) (LeafT (3 + k))) (NodeA (LeafT (2 + k)) (LeafT (4 + k)))
      r4 = travB t (\x acc -> acc + x) 0
      r5 = travB (NodeA (LeafT (7 + k)) (NodeB (3 + k) (LeafT (2 + k)))) (\x acc -> acc + x) 0
      r6 = evenStep (4 + k) (\x -> x)
  in r1 + r2 + r3 + r4 + r5 + r6

main :: IO ()
main = do
  n <- iterations 13913939
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 34 then "pass" else "fail")
