-- applicative-functor.sml (3cps test/programs/higher-order): applicative
-- pure/ap over option and an error-accumulating validation type, and a
-- traverse.  Literals get an opaque zero k added each round (Repeat.vary).
-- SML functions taking a pair are curried (apO, apV).
module Main (main) where

import Repeat

mapO :: (a -> b) -> Maybe a -> Maybe b
mapO _ Nothing = Nothing
mapO f (Just x) = Just (f x)

apO :: Maybe (a -> b) -> Maybe a -> Maybe b
apO (Just f) (Just x) = Just (f x)
apO _ _ = Nothing

liftA2O :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftA2O f a b = apO (mapO f a) b

liftA3O :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
liftA3O f a b c = apO (liftA2O f a b) c

traverseO :: (a -> Maybe b) -> [a] -> Maybe [b]
traverseO _ [] = Just []
traverseO f (x : rest) = liftA2O (\y ys -> y : ys) (f x) (traverseO f rest)

data Result a = Ok a | Err [Int]

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xr) ys = x : append xr ys

mapV :: (a -> b) -> Result a -> Result b
mapV _ (Err e) = Err e
mapV f (Ok x) = Ok (f x)

apV :: Result (a -> b) -> Result a -> Result b
apV (Ok f) (Ok x) = Ok (f x)
apV (Err e1) (Err e2) = Err (append e1 e2)
apV (Err e) _ = Err e
apV _ (Err e) = Err e

liftA2V :: (a -> b -> c) -> Result a -> Result b -> Result c
liftA2V f a b = apV (mapV f a) b

liftA3V :: (a -> b -> c -> d) -> Result a -> Result b -> Result c -> Result d
liftA3V f a b c = apV (liftA2V f a b) c

foldlH :: (a -> b -> b) -> b -> [a] -> b
foldlH _ acc [] = acc
foldlH f acc (x : xr) = foldlH f (f x acc) xr

sumList :: [Int] -> Int
sumList = foldlH (\x a -> x + a) 0

orElse :: Maybe a -> a -> a
orElse Nothing def = def
orElse (Just x) _ = x

total :: Int -> Int
total k =
  let r1 = orElse (liftA3O (\a b c -> a + b + c) (Just (1 + k)) (Just (2 + k)) (Just (3 + k))) (-1)
      r2 = orElse (liftA2O (\a b -> a + b) (Just (10 + k)) (if k == 0 then Nothing else Just k)) 0
      r3 = orElse (case traverseO (\x -> if x > 0 then Just (x * 2) else Nothing) [1 + k, 2 + k, 3 + k] of
                     Nothing -> Nothing
                     Just xs -> Just (sumList xs)) (-1)
      r4 = case traverseO (\x -> if x > 0 then Just x else Nothing) [1 + k, -1 + k, 3 + k] of
             Nothing -> 0
             Just _ -> -1
      r5 = case liftA3V (\a b c -> a + b + c) (Ok (4 + k)) (Ok (5 + k)) (Ok (6 + k)) of
             Ok x -> x
             Err _ -> -1
      r6 = case liftA2V (\a b -> a + b :: Int) (Err [1 + k, 2 + k]) (Err [3 + k]) of
             Ok _ -> -1
             Err es -> sumList es
  in r1 + r2 + r3 + r4 + r5 + r6

main :: IO ()
main = do
  n <- iterations 20000000   -- the SML: 2513421
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 39 then "pass" else "fail")
