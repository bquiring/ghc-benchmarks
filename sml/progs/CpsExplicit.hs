-- cps-explicit.sml (3cps test/programs/higher-order): hand-written CPS
-- (factorial, Fibonacci, map, fold) and a continuation saved in a ref and
-- invoked later, which runs in IO here.  Literals get an opaque zero k
-- added each round (Repeat.vary).
module Main (main) where

import Data.IORef
import Repeat

factK :: Int -> (Int -> a) -> a
factK n k = if n == 0 then k 1 else factK (n - 1) (\r -> k (n * r))

fibK :: Int -> (Int -> a) -> a
fibK n k = if n < 2 then k n else fibK (n - 1) (\a -> fibK (n - 2) (\b -> k (a + b)))

mapK :: (a -> (b -> r) -> r) -> [a] -> ([b] -> r) -> r
mapK _ [] k = k []
mapK f (x : rest) k = f x (\y -> mapK f rest (\ys -> k (y : ys)))

foldlK :: (b -> a -> (b -> r) -> r) -> b -> [a] -> (b -> r) -> r
foldlK _ acc [] k = k acc
foldlK f acc (x : rest) k = f acc x (\acc' -> foldlK f acc' rest k)

sumList :: [Int] -> Int
sumList [] = 0
sumList (x : rest) = x + sumList rest

-- The continuation escapes into a ref and is called after body returns
withSavedK :: Int -> (((Int -> Int) -> IO Int) -> IO Int) -> IO Int
withSavedK k body = do
  saved <- newIORef (\_ -> 0)
  _ <- body (\kk -> writeIORef saved kk >> return 0)
  f <- readIORef saved
  return (f (10 + k))

total :: Int -> IO Int
total k = do
  let r1 = factK (6 + k) (\x -> x)
      r2 = fibK (8 + k) (\x -> x)
      r3 = mapK (\x kk -> kk (x * 2)) [1 + k, 2 + k, 3 + k, 4 + k] sumList
      r4 = foldlK (\acc x kk -> kk (acc + x)) (0 + k) [1 + k, 2 + k, 3 + k, 4 + k, 5 + k] (\acc -> acc)
  r5 <- withSavedK k (\capture -> capture (\x -> x * 3) >> return 0)
  return (r1 + r2 + r3 + r4 + r5)

main :: IO ()
main = do
  n <- iterations 1367269
  let go i = do
        t <- total (vary i 0 0)
        if i >= n then return t else t `seq` go (i + 1)
  r <- go 1
  putStrLn (if r == 806 then "pass" else "fail")
