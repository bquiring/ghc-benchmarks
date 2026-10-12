-- closure-capture.sml (3cps test/programs/higher-order): closures capturing
-- variables from several enclosing scopes, sibling closures sharing free
-- variables, a captured variable that is never used.  Literals get an
-- opaque zero k added each round (Repeat.vary).
module Main (main) where

import Repeat

outer :: Int -> Int -> Int -> Int
outer a = let b = a + 1
          in \c -> let d = b * 2 in \e -> a + b + c + d + e

makeSiblings :: Int -> Int -> (Int -> Int, Int -> Int, Int -> Int)
makeSiblings x y = (\z -> x + y + z, \z -> x * z, \z -> x + y + x * z)

withUseless :: Int -> Int -> Int
withUseless a = let _useless = a * 1000
                    useful = a + 7
                in \x -> useful + x

foldlH :: (a -> b -> b) -> b -> [a] -> b
foldlH _ acc [] = acc
foldlH f acc (x : xr) = foldlH f (f x acc) xr

total :: Int -> Int
total k =
  let inner = outer (3 + k) (10 + k)
      r1 = inner (5 + k)
      (f, g, h) = makeSiblings (3 + k) (4 + k)
      r2 = f (1 + k)
      r3 = g (2 + k)
      r4 = h (1 + k)
      wf = withUseless (5 + k)
      r5 = wf (8 + k)
      cls = [inner, wf]
      r6 = foldlH (\f' acc -> acc + f' (1 + k)) 0 cls
  in r1 + r2 + r3 + r4 + r5 + r6

main :: IO ()
main = do
  n <- iterations 26069491
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 113 then "pass" else "fail")
