-- hamming.ml (OCaml testsuite tests/misc): the Hamming numbers as a lazy
-- stream merged from three scaled copies of itself (explicitly lazy in
-- OCaml; Haskell's own laziness here), with 37-digit numbers made of two
-- Int64 halves.  Prints the 88000th to 88099th, as the OCaml does.  Each
-- round builds the stream afresh (in OCaml it is a top-level value).
module Main (main) where

import Data.Int (Int64)
import Text.Printf (printf)
import Repeat

digit :: Int64
digit = 1000000000000000000

type Num' = (Int64, Int64)   -- (low 18 digits, high part)

mul :: Int64 -> Num' -> Num'
mul n (pl, ph) = ((n * pl) `rem` digit, n * ph + (n * pl) `quot` digit)

cmp :: Num' -> Num' -> Int
cmp (nl, nh) (pl, ph)
  | nh < ph = -1
  | nh > ph = 1
  | nl < pl = -1
  | nl > pl = 1
  | otherwise = 0

pr :: Num' -> String
pr (nl, nh) = if nh == 0 then printf "%d" nl else printf "%d%018d" nh nl

data LCons a = Cons a (LCons a)

mapL :: (a -> b) -> LCons a -> LCons b
mapL f (Cons x ll) = Cons (f x) (mapL f ll)

merge :: (a -> a -> Int) -> LCons a -> LCons a -> LCons a
merge c l1@(Cons x1 ll1) l2@(Cons x2 ll2) =
  let r = c x1 x2
  in if r == 0 then Cons x1 (merge c ll1 ll2)
     else if r < 0 then Cons x1 (merge c ll1 l2)
     else Cons x2 (merge c l1 ll2)

iterInterval :: (a -> b) -> LCons a -> (Int, Int) -> [b]
iterInterval f (Cons x ll) (start, stop)
  | stop == 0 = []
  | otherwise = (if start <= 0 then (f x :) else id) (iterInterval f ll (start - 1, stop - 1))

hamming :: Int64 -> LCons Num'
hamming one = h
  where h = Cons (one, 0) (merge cmp ham2 (merge cmp ham3 ham5))
        ham2 = mapL (mul 2) h
        ham3 = mapL (mul 3) h
        ham5 = mapL (mul 5) h

main :: IO ()
main = do
  n <- iterations 120
  let outs = [ iterInterval pr (hamming (vary i 1 0)) (88000, 88100) | i <- [1 .. n] ]
  mapM_ putStrLn (if all (== head outs) outs then head outs else ["fail"])
