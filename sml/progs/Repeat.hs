-- | Shared by the translations.
--
-- SML programs often run their work n times with a loop that keeps the
-- last result.  SML is strict, so every iteration's result is fully
-- computed; 'loopLast' does the same (deepseq).  Each iteration's input goes
-- through 'vary', which the optimiser cannot see through (NOINLINE), so the
-- work cannot be floated out of the loop and shared between iterations.
module Repeat (vary, loopLast, repeatAll, iterations) where

import Control.DeepSeq (NFData, deepseq)
import System.Environment (getArgs)

-- | @vary i x y == x@ for every i >= 0, but the compiler cannot know it
vary :: Int -> a -> a -> a
vary i x y = if i < 0 then y else x
{-# NOINLINE vary #-}

-- | Iteration count: the first command-line argument, or the default
iterations :: Int -> IO Int
iterations dflt = do
  as <- getArgs
  return (case as of { (a : _) -> read a; [] -> dflt })

-- | Compute @f i@ for i = 1..n, each fully evaluated, and return the last
loopLast :: NFData a => Int -> (Int -> a) -> a
loopLast n f = go 1
  where go i = let r = f i
               in if i >= n then r else r `deepseq` go (i + 1)

-- | Run @f i@ for i = 1..n; True if every run satisfied the check
repeatAll :: Int -> (Int -> Bool) -> Bool
repeatAll n f = go 1
  where go i | i > n     = True
             | otherwise = f i && go (i + 1)
