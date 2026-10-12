-- tree-fold-ho.sml (3cps test/programs/higher-order): a generic fold over a
-- tree, instantiated with several function pairs, and a map layered on it.
-- The tree size gets an opaque zero k added each round (Repeat.vary).
module Main (main) where

import Repeat

data Tree = Leaf Int | Node Tree Tree

foldTree :: (Int -> b) -> (b -> b -> b) -> Tree -> b
foldTree leaf node t = case t of
  Leaf x -> leaf x
  Node l r -> node (foldTree leaf node l) (foldTree leaf node r)

mapTree :: (Int -> Int) -> Tree -> Tree
mapTree f t = case t of
  Leaf x -> Leaf (f x)
  Node l r -> Node (mapTree f l) (mapTree f r)

maxInt :: Int -> Int -> Int
maxInt a b = if a > b then a else b

buildTree :: Int -> Tree
buildTree 0 = Leaf 1
buildTree 1 = Node (Leaf 1) (Leaf 1)
buildTree n = Node (buildTree (n - 1)) (buildTree (n - 2))

total :: Int -> Int
total k =
  let t = buildTree (5 + k)
      tsum = foldTree (\x -> x) (\l r -> l + r) t
      tcount = foldTree (\_ -> 1) (\l r -> l + r) t
      tdepth = foldTree (\_ -> 1) (\l r -> 1 + maxInt l r) t
      t2 = mapTree (\x -> x + 1) t
      tsum2 = foldTree (\x -> x) (\l r -> l + r) t2
  in tsum + tcount + tdepth + tsum2

main :: IO ()
main = do
  n <- iterations 1622010
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 58 then "pass" else "fail")
