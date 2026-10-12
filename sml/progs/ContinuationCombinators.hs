-- continuation-combinators.sml (3cps test/programs/higher-order): a
-- continuation monad (answer type Int) and a state monad built from
-- closures, and a continuation saved in a ref (IO here).  Literals get an
-- opaque zero k added each round (Repeat.vary).
module Main (main) where

import Data.IORef
import Repeat

type M a = (a -> Int) -> Int

mReturn :: a -> M a
mReturn x k = k x

mBind :: M a -> (a -> M b) -> M b
mBind m f k = m (\x -> f x k)

mRun :: M Int -> Int
mRun m = m (\x -> x)

type St a = Int -> (a, Int)

stReturn :: a -> St a
stReturn x s = (x, s)

stBind :: St a -> (a -> St b) -> St b
stBind m f s = let (a, s') = m s in f a s'

stGet :: St Int
stGet s = (s, s)

stPut :: Int -> St ()
stPut ns = \_ -> ((), ns)

stRun :: St a -> Int -> a
stRun m ini = let (v, _) = m ini in v

withEscapingK :: Int -> (((Int -> Int) -> IO Int) -> IO Int) -> IO Int
withEscapingK k body = do
  cell <- newIORef (\x -> x)
  _ <- body (\kk -> writeIORef cell kk >> return 0)
  f <- readIORef cell
  return (f (7 + k))

total :: Int -> IO Int
total k = do
  let r1 = mRun (mBind (mReturn (3 + k)) (\a -> mBind (mReturn (4 + k)) (\b -> mReturn (a + b))))
      r2 = mRun (mBind (mReturn (10 + k)) (\a ->
                 mBind (mReturn (5 + k)) (\b ->
                 mBind (mReturn (a - b)) (\c ->
                 mReturn (a + b + c)))))
      r3 = stRun (stBind stGet (\a ->
                  stBind (stPut (a + 5)) (\_ ->
                  stBind stGet (\b ->
                  stReturn (a + b))))) (10 + k)
  r4 <- withEscapingK k (\capture -> capture (\x -> x * 3) >> return 0)
  return (r1 + r2 + r3 + r4)

main :: IO ()
main = do
  n <- iterations 150000000   -- the SML: 5644178
  let go i = do
        t <- total (vary i 0 0)
        if i >= n then return t else t `seq` go (i + 1)
  r <- go 1
  putStrLn (if r == 73 then "pass" else "fail")
