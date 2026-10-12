-- sieve.ml (OCaml testsuite tests/misc): Eratosthenes' sieve on lists,
-- filtering out the multiples of each prime up to the square root of
-- 50000.  Prints the primes, as the OCaml does.
module Main (main) where

import Repeat

interval :: Int -> Int -> [Int]
interval mn mx = if mn > mx then [] else mn : interval (mn + 1) mx

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (a : r) = if p a then a : filter' p r else filter' p r

removeMultiplesOf :: Int -> [Int] -> [Int]
removeMultiplesOf n = filter' (\m -> m `mod` n /= 0)

sieve :: Int -> [Int]
sieve mx = filterAgain (interval 2 mx)
  where filterAgain [] = []
        filterAgain l@(n : r) = if n * n > mx then l else n : filterAgain (removeMultiplesOf n r)

main :: IO ()
main = do
  n <- iterations 300
  let outs = [ sieve (vary i 50000 0) | i <- [1 .. n] ]
  putStrLn (if all (== head outs) outs then concatMap (\p -> ' ' : show p) (head outs) else "fail")
