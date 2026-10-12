-- derivative.sml (3cps test/programs/llvm-sml), from the Larceny DERIV
-- benchmark: symbolic differentiation, repeated.  Like the SML, each round
-- also makes a few calls that raise (and are caught, printing nothing),
-- and ends with a test that prints pass or fail.
module Main (main) where

import Control.Exception
import Repeat

data UnequalLengths = UnequalLengths deriving Show
instance Exception UnequalLengths

data NoDerivation = NoDerivation deriving Show
instance Exception NoDerivation

-- SML's ListPair.foldlEq: raises on unequal lengths
foldlEq :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldlEq f ini l1 l2 = foldf l1 l2 ini
  where foldf (x : xs) (y : ys) accum = foldf xs ys (f x y accum)
        foldf [] [] accum = accum
        foldf _ _ _ = throw UnequalLengths

data Expr = Add [Expr] | Sub [Expr] | Mul [Expr] | Div [Expr] | Num Int | X

equal :: Expr -> Expr -> Bool
equal X X = True
equal (Num a) (Num b) = a == b
equal (Add xs) (Add ys) = chkLists xs ys
equal (Sub xs) (Sub ys) = chkLists xs ys
equal (Mul xs) (Mul ys) = chkLists xs ys
equal (Div xs) (Div ys) = chkLists xs ys
equal _ _ = False

chkLists :: [Expr] -> [Expr] -> Bool
chkLists = foldlEq chk True
  where chk a b acc = acc && equal a b

deriv :: Expr -> Expr
deriv a = case a of
  X -> Num 1
  Num _ -> Num 0
  Add es -> Add (map deriv es)
  Sub es -> Sub (map deriv es)
  Mul es -> Mul [a, Add (map (\x -> Div [deriv x, x]) es)]
  Div [x, y] -> Sub [Div [deriv x, y], Div [x, Mul [y, y, deriv y]]]
  _ -> throw NoDerivation

-- What test prints
test :: Expr -> Expr -> String
test ans e = if equal ans (deriv e) then "pass" else "fail"

-- Evaluate; Nothing if it raised (SML's  e handle _ => ...)
attempt :: a -> IO (Maybe a)
attempt ~x = either (\(SomeException _) -> Nothing) Just <$> try (evaluate x)

-- The lines one round prints
doit :: Int -> IO [String]
doit i = do
  let mkExp a b = Sub [Add [Mul [Num 3, X, X], Mul [a, X, X], Mul [b, X], Num 5], X]
      mkAns a b = Sub [Add [ Mul [Mul [Num 3, X, X], Add [Div [Num 0, Num 3], Div [Num 1, X], Div [Num 1, X]]]
                           , Mul [Mul [a, X, X], Add [Div [Num 0, a], Div [Num 1, X], Div [Num 1, X]]]
                           , Mul [Mul [b, X], Add [Div [Num 0, b], Div [Num 1, X]]]
                           , Num 0 ], Num 1]
      a = Num (vary i 5 0)
      b = Num 7
      ex = mkExp a b
      ans = mkAns a b
  t1 <- attempt (test ans ans)
  t2 <- attempt (test ans ans)
  _ <- evaluate (equal (Num 2) (Num 1))
  _ <- evaluate (equal X (Num 1))
  _ <- evaluate (equal (Num 1) X)
  _ <- evaluate (equal (Add []) (Sub []))
  _ <- evaluate (equal (Sub []) (Add []))
  _ <- evaluate (equal (Mul []) (Div []))
  _ <- evaluate (equal (Div []) (Mul []))
  _ <- attempt (foldlEq (\x y acc -> acc && x == (y :: Int)) True ([] :: [Int]) [1])
  _ <- attempt (case deriv (Div []) of { X -> (); _ -> () })
  t3 <- evaluate (test ans ex)
  return (maybe [] pure t1 ++ maybe [] pure t2 ++ [t3])

-- Each round prints what the SML's round prints; the output is one round's
-- lines, if every round's are the same
main :: IO ()
main = do
  n <- iterations 1500000
  first <- doit 1
  same <- allM (\i -> (== first) <$> doit i) [2 .. n]
  mapM_ putStrLn (if same then first else ["fail: rounds differ"])
  where allM _ [] = return True
        allM p (x : xs) = p x >>= \b -> if b then allM p xs else return False
