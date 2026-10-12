{-# LANGUAGE Strict #-}
-- nqueens.sml (3cps test/programs/llvm-sml): count the solutions of the
-- n-queens problem, with the SML's own list functions.
module Main (main) where

import Repeat

revAppend :: [a] -> [a] -> [a]
revAppend [] ys = ys
revAppend (x : xr) ys = revAppend xr (x : ys)

append :: [a] -> [a] -> [a]
append [] ys = ys
append xs [] = xs
append xs ys = revAppend (revAppend xs []) ys

len :: [a] -> Int
len xs0 = lp xs0 0
  where lp [] n = n
        lp (_ : r) n = lp r (n + 1)

nsoln :: Int -> Int
nsoln nq = len (gen nq)
  where
    safe :: Int -> Int -> [Int] -> Bool
    safe _ _ [] = True
    safe x d (q : l) = x /= q && x /= q + d && x /= q - d && safe x (d + 1) l
    gen :: Int -> [[Int]]
    gen 0 = [[]]
    gen n = enumerate nq []
      where
        bs0 = gen (n - 1)
        enumerate 0 acc = acc
        enumerate q acc = enumerate (q - 1) (append (check bs0 []) acc)
          where check [] acc' = acc'
                check (b : bs) acc'
                  | safe q 1 b = check bs ((q : b) : acc')
                  | otherwise  = check bs acc'

main :: IO ()
main = do
  n <- iterations 4
  let ok i = nsoln (vary i 5 0) == 10 && nsoln (vary i 12 0) == 14200
  putStrLn (if repeatAll n ok then "pass" else "fail")
