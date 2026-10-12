-- twenty-four.sml (3cps test/programs/llvm-sml): a solver for the game of
-- 24 in continuation-passing style: every backtracking function takes a
-- success continuation (result and a resume function) and a failure
-- continuation.  Counts the solutions for all 21 two-card hands from 1..7.
module Main (main) where

import Repeat

data Expr = Const Int
          | Sum Expr Expr
          | Diff Expr Expr
          | Prod Expr Expr
          | Quot Expr Expr

eval :: Expr -> Double
eval (Const c) = fromIntegral c
eval (Sum e1 e2) = eval e1 + eval e2
eval (Diff e1 e2) = eval e1 - eval e2
eval (Prod e1 e2) = eval e1 * eval e2
eval (Quot e1 e2) = eval e1 / eval e2   -- div 0 will be inf

splitAt' :: [a] -> Int -> ([a], [a])
splitAt' xs0 n0 = split n0 xs0 []
  where split 0 suffix rPrefix = (reverse rPrefix, suffix)
        split _ [] _ = error "Subscript"
        split i (x : xs) rPrefix = split (i - 1) xs (x : rPrefix)

type Succ r a = a -> (() -> r) -> r
type Fail r = () -> r

-- Split a list into a pair of two non-empty lists
partition :: [a] -> Succ r ([a], [a]) -> Fail r -> r
partition xs succ0 fail0 = p 1 succ0 fail0
  where len = length xs
        p n succ fl
          | n < len   = succ (splitAt' xs n) (\() -> p (n + 1) succ fl)
          | otherwise = fl ()

-- Combine two expressions with all four operators
combineExps :: Expr -> Expr -> Succ r Expr -> Fail r -> r
combineExps e1 e2 succ fl =
  succ (Sum e1 e2) (\() ->
  succ (Diff e1 e2) (\() ->
  succ (Prod e1 e2) (\() ->
  succ (Quot e1 e2) fl)))

-- All expression trees over a list of expressions
allTrees :: [Expr] -> Succ r Expr -> Fail r -> r
allTrees [] _ _ = error "impossible"
allTrees [x] succ fl = succ x fl
allTrees xs succ fl =
  partition xs
    (\(left, right) resume ->
      allTrees left (\leftTree resume1 ->
        allTrees right (\rightTree resume2 ->
          combineExps leftTree rightTree succ resume2)
        resume1)
      resume)
    fl

realEqual :: Double -> Double -> Bool
realEqual x y = not (x < y) && not (x > y)

checkExp :: Expr -> Succ r Expr -> Fail r -> r
checkExp e succ fl = if realEqual (eval e) 24.0 then succ e fl else fl ()

-- Insert v into all positions of a list
insert :: a -> [a] -> Succ r [a] -> Fail r -> r
insert v [] succ fl = succ [v] fl
insert v lst@(x : xs) succ fl =
  succ (v : lst) (\() -> insert v xs (\res resume -> succ (x : res) resume) fl)

permutations :: [a] -> Succ r [a] -> Fail r -> r
permutations [] succ fl = succ [] fl
permutations (x : xs) succ fl =
  permutations xs (\res resume -> insert x res succ resume) fl

solve :: [Int] -> Succ r Expr -> Fail r -> r
solve nums succ fl =
  permutations (map Const nums)
    (\perm resume -> allTrees perm (\e resume1 -> checkExp e succ resume1) resume)
    fl

allHands :: Int -> Int -> [[Int]]
allHands curr left
  | left < 0 || left > curr = []
  | curr == 0 = [[]]
  | otherwise = map (curr :) (allHands (curr - 1) (left - 1)) ++ allHands (curr - 1) left

countSolutions :: [Int] -> Int
countSolutions hand = solve hand (\_ resume -> 1 + resume ()) (\() -> 0)

main :: IO ()
main = do
  n <- iterations 300000
  let result = loopLast n (\i -> foldr (\hand acc -> countSolutions hand : acc) [] (allHands (vary i 7 0) 2))
  putStrLn (if result == [0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0] then "pass" else "fail")
