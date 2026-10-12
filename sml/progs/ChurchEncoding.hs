-- church-encoding.sml (3cps test/programs/higher-order): Church numerals
-- (specialised to Int), booleans and pairs.  Realising a numeral starts
-- from an opaque zero k each round (Repeat.vary), as do the other literals.
module Main (main) where

import Repeat

type N = (Int -> Int) -> Int -> Int

csucc :: N -> N
csucc n f x = f (n f x)

cplus, cmult :: N -> N -> N
cplus m n f x = m f (n f x)
cmult m n f x = m (n f) x

c1, c2, c3, c4 :: N
c1 f x = f x
c2 f x = f (f x)
c3 f x = f (f (f x))
c4 f x = f (f (f (f x)))

ctrue, cfalse :: Int -> Int -> Int
ctrue t _ = t
cfalse _ f = f

cboolObs :: Int -> (Int -> Int -> Int) -> Int
cboolObs k b = b (1 + k) (0 + k)

cpair :: Int -> Int -> (Int -> Int -> a) -> a
cpair a b s = s a b

cfst, csnd :: ((Int -> Int -> Int) -> Int) -> Int
cfst p = p (\a _ -> a)
csnd p = p (\_ b -> b)

total :: Int -> Int
total k =
  let realize n = n (\x -> x + 1) (0 + k)
      r1 = realize (cplus c3 c4)
      r2 = realize (cmult c3 c4)
      r3 = realize (csucc c4)
      r4 = realize (cplus (cmult c2 c3) c1)
      r5 = cboolObs k ctrue
      r6 = cboolObs k cfalse
      r7 = ctrue r1 r2
      p = cpair (10 + k) (20 + k)
      r8 = cfst p
      r9 = csnd p
      p2 = cpair r1 r2
      r10 = cfst p2 + csnd p2
  in r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 + r10

main :: IO ()
main = do
  n <- iterations 1000000000   -- the SML: 5623379; each round is a few additions
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 7+12+5+7+1+0+7+10+20+19 then "pass" else "fail")
