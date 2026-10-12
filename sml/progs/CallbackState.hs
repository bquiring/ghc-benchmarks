-- callback-state.sml (3cps test/programs/higher-order): closures over
-- mutable refs: independent counters, a shared cell, a stateful callback,
-- a memoiser.  Runs in IO (refs are IORefs), with fresh refs each round as
-- in the SML.  Literals get an opaque zero k added each round.
module Main (main) where

import Data.IORef
import Repeat

makeCounter :: Int -> IO (() -> IO Int)
makeCounter k = do
  c <- newIORef (0 + k)
  return (\() -> modifyIORef c (+ 1) >> readIORef c)

makeSharedPair :: Int -> IO (() -> IO (), () -> IO Int)
makeSharedPair k = do
  cell <- newIORef (0 + k)
  return (\() -> modifyIORef cell (+ 1), \() -> readIORef cell)

accumulate :: Int -> (Int -> (Int -> IO ()) -> IO ()) -> [Int] -> IO Int
accumulate k f xs0 = do
  acc <- newIORef (0 + k)
  let go [] = return ()
      go (x : rest) = f x (\v -> modifyIORef acc (+ v)) >> go rest
  go xs0
  readIORef acc

memo :: (Int -> Int) -> IO (Int -> IO Int)
memo f = do
  cache <- newIORef Nothing
  return (\x -> readIORef cache >>= \c -> case c of
    Just v -> return v
    Nothing -> let v = f x in writeIORef cache (Just v) >> return v)

total :: Int -> IO Int
total k = do
  ctr1 <- makeCounter k
  ctr2 <- makeCounter k
  _ <- ctr1 (); _ <- ctr1 (); _ <- ctr1 ()
  _ <- ctr2 (); _ <- ctr2 ()
  r1 <- ctr1 ()
  r2 <- ctr2 ()
  (inc, get) <- makeSharedPair k
  inc (); inc (); inc ()
  r3 <- get ()
  r4 <- accumulate k (\x kk -> kk (x * 2)) [1 + k, 2 + k, 3 + k]
  memFn <- memo (\x -> x * x + x)
  r5 <- memFn (5 + k)
  r6 <- memFn (5 + k)
  return (r1 + r2 + r3 + r4 + r5 + r6)

main :: IO ()
main = do
  n <- iterations 18306267
  let go i = do
        t <- total (vary i 0 0)
        if i >= n then return t else t `seq` go (i + 1)
  r <- go 1
  putStrLn (if r == 82 then "pass" else "fail")
