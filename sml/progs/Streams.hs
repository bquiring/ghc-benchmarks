{-# LANGUAGE Strict #-}
-- streams.sml (3cps test/programs/llvm-sml): pull streams (a state and a
-- step function) and push streams (continuations), zipped together,
-- 100000 times.  SML's tuple-argument functions are curried here.
module Main (main) where

import Repeat

-- Pull streams
data Step a s = Yield a s | Skip s | Done

data Pull s a = Pull s (s -> Step a s)

pFromList :: [a] -> Pull [a] a
pFromList xs = Pull xs (\l -> case l of { [] -> Done; x : r -> Yield x r })

pMap :: (a -> b) -> Pull s a -> Pull s b
pMap f (Pull s step) = Pull s (\s1 -> case step s1 of
                                 Done -> Done
                                 Skip s' -> Skip s'
                                 Yield a s' -> Yield (f a) s')

pTake :: Pull s a -> Int -> Pull (s, Int) a
pTake (Pull s step) n0 = Pull (s, n0) (\(s1, n) ->
  if n <= 0 then Done else
  case step s1 of
    Done -> Done
    Skip s' -> Skip (s', n)
    Yield a s' -> Yield a (s', n - 1))

pFilter :: (a -> Bool) -> Pull s a -> Pull s a
pFilter p (Pull s step) = Pull s (\s1 -> case step s1 of
                                    Done -> Done
                                    Skip s' -> Skip s'
                                    Yield a s' -> if p a then Yield a s' else Skip s')

-- Push streams: given a cons and a nil continuation, produce the result
type Push s a = (a -> (() -> s) -> s) -> (() -> s) -> s

sFromList :: [a] -> Push s a
sFromList xs c n = go xs
  where go [] = n ()
        go (x : r) = c x (\_ -> go r)

sZipWith :: (a -> b -> c) -> Push (st -> r) a -> Pull st b -> Push r c
sZipWith f k (Pull seed step) c n =
  k (\a hyp s -> case step s of
                   Done -> hyp () s
                   Skip s' -> hyp () s'
                   Yield b s' -> c (f a b) (\() -> hyp () s'))
    (\() _ -> n ())
    seed

sFoldl :: (a -> b -> b) -> b -> Push b a -> b
sFoldl f acc k = k (\a s -> f a (s ())) (\_ -> acc)

complex :: [Int] -> [Int] -> [(Int, Int)]
complex xs0 ys0 =
  let xs = pMap (\x -> x * x) (pFilter (\n -> n `mod` 2 == 0) (pTake (pMap (\x -> x * x) (pFromList xs0)) 12))
      ys = sFromList ys0
      s = sZipWith (\a b -> (a, b)) ys xs
  in sFoldl (:) [] s

s1, s2 :: [Int]
s1 = [1, 2, 3, 4, 5, 6]
s2 = [10, 20, 30, 40, 50, 60]

main :: IO ()
main = do
  n <- iterations 30000000
  let result = loopLast n (\i -> complex (vary i s1 []) s2)
  putStrLn (if result == [(20, 16), (40, 256), (60, 1296)] then "pass" else "fail")
