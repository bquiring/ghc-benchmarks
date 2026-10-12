-- interpreter.sml (3cps test/programs/llvm-sml): an interpreter for a
-- small language with closures, letrec and a store, run on three programs
-- 10000 times.
module Main (main) where

import Repeat

len :: [a] -> Int
len xs0 = lp xs0 0
  where lp [] n = n
        lp (_ : r) n = lp r (n + 1)

-- Lazy failing, as in the SML: unequal lengths stop early
foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 f base l1 l2 = case (l1, l2) of
  ([], []) -> base
  ([], _) -> base
  (_, []) -> base
  (a : rst1, b : rst2) -> f a b (foldr2 f base rst1 rst2)

foldlS :: (a -> b -> b) -> b -> [a] -> b
foldlS f base lst = case lst of
  [] -> base
  a : rst -> foldlS f (f a base) rst

revAppend :: [a] -> [a] -> [a]
revAppend [] ys = ys
revAppend (x : xr) ys = revAppend xr (x : ys)

rev :: [a] -> [a]
rev xs = revAppend xs []

type Var = Int
data Prim = Plus | Times | Assign | Ref | Deref

data Exp = Int Int
         | Var Var
         | Fun ([Int], Exp)
         | Call Exp [Exp]
         | IfZero Exp Exp Exp
         | Letrec [(Int, ([Int], Exp))] Exp
         | Prim Prim

type Address = Int
type Env = [(Var, Address)]

data Value = VInt Int
           | VClo ([Int], Exp) Env
           | VPrim Prim
           | VCell Address

type Store = [(Address, Value)]

extend :: [(a, b)] -> a -> b -> [(a, b)]
extend m a b = (a, b) : m

findEnv :: Env -> Var -> Address -> Address
findEnv m a b = case m of
  [] -> b
  (a', b') : rst -> if a == a' then b' else findEnv rst a b

findStore :: Store -> Address -> Value -> Value
findStore m a b = case m of
  [] -> b
  (a', b') : rst -> if a == a' then b' else findStore rst a b

interpret :: Exp -> Value
interpret e0 = case aux e0 initialEnv initialStore initialAddress of (final, _, _) -> final
  where
    dummyAddress = 0
    dummyValue = VInt 0
    initialEnv = []
    initialStore = [(dummyAddress, dummyValue)]
    initialAddress = dummyAddress + 1
    lookupEnv env x = findEnv env x dummyAddress
    lookupStore store a = findStore store a dummyValue
    dummy store n = (dummyValue, store, n)
    makePrim prim vs store n = case (prim, vs) of
      (Plus, [VInt i1, VInt i2]) -> (VInt (i1 + i2), store, n)
      (Times, [VInt i1, VInt i2]) -> (VInt (i1 * i2), store, n)
      (Ref, [v]) -> (VCell n, extend store n v, n + 1)
      (Deref, [VCell n']) -> (lookupStore store n', store, n)
      (Assign, [VCell n', v]) -> (lookupStore store n', extend store n' v, n)
      _ -> dummy store n
    aux :: Exp -> Env -> Store -> Address -> (Value, Store, Address)
    aux e env store n = case e of
      Int i -> (VInt i, store, n)
      Var x -> (lookupStore store (lookupEnv env x), store, n)
      Fun lam -> (VClo lam env, store, n)
      Call f args ->
        let (f', store1, n1) = aux f env store n
            (argsRev, store2, n2) =
              foldlS (\arg (as', st, k) -> let (arg', st', k') = aux arg env st k
                                           in (arg' : as', st', k'))
                     ([], store1, n1) args
            args' = rev argsRev
        in case f' of
             VClo (params, body) cenv ->
               if len argsRev == len params
                 then let (env', store3, n3) =
                            foldr2 (\a b (en, st, k) -> (extend en a k, extend st k b, k + 1))
                                   (cenv, store2, n2) params args'
                      in aux body env' store3 n3
                 else dummy store2 n2
             VPrim prim -> makePrim prim args' store2 n2
             _ -> dummy store2 n2
      IfZero tst thn els ->
        let (tst', store1, n1) = aux tst env store n
        in case tst' of
             VInt 0 -> aux thn env store1 n1
             _ -> aux els env store1 n1
      Letrec binds body ->
        let (n', env') = foldlS (\(x, _) (k, en) -> (k + 1, extend en x k)) (n, env) binds
            store' = foldlS (\(x, lam) st -> extend st (lookupEnv env' x) (VClo lam env')) store binds
        in aux body env' store' n'
      Prim prim -> (VPrim prim, store, n)

prog6 :: Exp
prog6 = Letrec [(1, ([2], Letrec [(3, ([4], IfZero (Var 4) (Var 2) (Int (-1))))] (Var 3)))]
               (Call (Call (Var 1) [Int 1]) [Int 10])

primRef, primDeref, primAssign :: Exp
primRef = Fun ([1, 2], Call (Var 1) [Call (Prim Ref) [Var 2]])
primDeref = Fun ([1, 2], Call (Var 1) [Call (Prim Deref) [Var 2]])
primAssign = Fun ([1, 2, 3], Call (Var 1) [Call (Prim Assign) [Var 2, Var 3]])

prog7 :: Exp
prog7 =
  Call (Fun ([101],
             Call primRef
                  [Fun ([1],
                        Call (Fun ([102, 0],
                                   Call (Fun ([104],
                                              Call (Fun ([7],
                                                         Call primDeref
                                                              [Fun ([6], IfZero (Var 6)
                                                                                (Call (Var 104) [Int 0])
                                                                                (Call (Var 104) [Var 7])),
                                                               Var 0]))
                                                   [Fun ([105, 8], Call (Var 104) [Var 8])]))
                                        [Fun ([5],
                                              Call primAssign
                                                   [Fun ([2],
                                                         Call (Fun ([103, -1],
                                                                    Call primDeref
                                                                         [Fun ([3],
                                                                               IfZero (Var 3)
                                                                                      (Call (Var 103) [Int 42])
                                                                                      (Call primDeref
                                                                                            [Fun ([4], Call (Var 4) [Var 103, Int 0]),
                                                                                             Var 0])),
                                                                          Var 0]))
                                                              [Var 102, Var 2]),
                                                    Var 0,
                                                    Var 5])]))
                             [Var 101, Var 1]),
                   Fun ([100, 0], Call (Var 100) [Var 0])]))
       [Fun ([1], Var 1)]

prog8 :: Exp
prog8 = IfZero (Int 0) (Int 1) (Int 2)

check :: Value -> Int -> String
check (VInt v') a = if v' == a then "pass" else "fail"
check _ _ = "fail"

data R = R Value Value Value

main :: IO ()
main = do
  n <- iterations 300000
  let R r6 r7 r8 = loopLastR n
      loopLastR k = go 1
        where go i = let r = R (interpret (vary i prog6 prog6)) (interpret (vary i prog7 prog7)) (interpret (vary i prog8 prog8))
                     in if i >= k then r else forceR r `seq` go (i + 1)
      forceR (R a b c) = a `seq` b `seq` c `seq` ()
  mapM_ putStrLn [check r6 (-1), check r7 42, check r8 1]
