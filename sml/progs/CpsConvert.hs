{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- cps-convert.sml (3cps test/programs/llvm-sml; John Reppy): the
-- Danvy-Filinski CPS conversion from "Representing Control", with the
-- meta-level continuations as functions, 100000 times.  (The SML's
-- top-level "extra coverage" calls, run once at load, are left out.)
module Main (main) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Repeat

data Prim = ADD | SUB | MUL | DIV deriving (Eq, Generic, NFData)

-- Var
data Var = V String Int deriving (Eq, Generic, NFData)
newtype VarGen = VGen Int deriving (Eq, Generic, NFData)

newVar :: VarGen -> String -> (Var, VarGen)
newVar (VGen n) s = (V s n, VGen (n + 1))

nameOf :: Var -> String
nameOf (V s _) = s

compareVar :: Var -> Var -> Ordering
compareVar (V _ n1) (V _ n2)
  | n1 < n2 = LT
  | n1 > n2 = GT
  | otherwise = EQ

-- Finite maps as sorted lists
newtype VMap a = VMap [(Var, a)]

vmInsert :: VMap a -> Var -> a -> VMap a
vmInsert (VMap items) x v = VMap (ins items)
  where ins [] = [(x, v)]
        ins its@((x', v') : r) = case compareVar x x' of
          LT -> (x, v) : its
          EQ -> (x, v) : r
          GT -> (x', v') : ins r

vmFind :: VMap a -> Var -> Maybe a
vmFind (VMap items) x = find' items
  where find' [] = Nothing
        find' ((y, v) : r) = case compareVar x y of
          LT -> Nothing
          EQ -> Just v
          GT -> find' r

-- Lambda
data Lam = LVAR Var | LLAMBDA Var Lam | LAPP Lam Lam | LPRIM Prim [Lam] | LIF Lam Lam Lam

-- CVar
data CVar = CV String Int deriving (Eq, Generic, NFData)
newtype CVarGen = CGen Int deriving (Eq, Generic, NFData)

newCVar' :: CVarGen -> String -> (CVar, CVarGen)
newCVar' (CGen n) s = (CV s n, CGen (n + 1))

-- CPS
data CExp = UAPP UVal UVal CVal
          | CAPP CVal UVal
          | PAPP Prim [UVal] CVal
          | CIF UVal CExp CExp
          | LETC CVar CVal CExp
          deriving (Eq, Generic, NFData)
data UVal = UVAR Var | ULAMBDA Var CVar CExp deriving (Eq, Generic, NFData)
data CVal = CVAR CVar | CLAMBDA Var CExp | HALT deriving (Eq, Generic, NFData)

-- Convert
newtype Env = Env (VMap Var)

emptyEnv :: Env
emptyEnv = Env (VMap [])

data Gen = Gen VarGen CVarGen deriving (Eq, Generic, NFData)

newGen :: Gen
newGen = Gen (VGen 0) (CGen 0)

newUVar :: Gen -> String -> (Var, Gen)
newUVar (Gen vgen cgen) name = let (k, vgen') = newVar vgen name in (k, Gen vgen' cgen)

newCVar :: Gen -> String -> (CVar, Gen)
newCVar (Gen vgen cgen) name = let (k, cgen') = newCVar' cgen name in (k, Gen vgen cgen')

findVar :: Env -> Var -> UVal
findVar (Env vmap) x = case vmFind vmap x of
  Just x' -> UVAR x'
  Nothing -> error "unbound variable"

bindVar :: Env -> Gen -> Var -> (Var, Env, Gen)
bindVar (Env vmap) gen x = let (x', gen') = newUVar gen (nameOf x)
                           in (x', Env (vmInsert vmap x x'), gen')

-- Conversion for expressions in tail position (never given a CLAMBDA, so
-- it is safe to use k twice in the conversion of IF)
tailExpToExp :: Env -> Gen -> Lam -> CVal -> (CExp, Gen)
tailExpToExp env gen e k = case e of
  LVAR x -> (CAPP k (findVar env x), gen)
  LLAMBDA x body ->
    let (x', env', gen1) = bindVar env gen x
        (k', gen2) = newCVar gen1 "k"
        (body', gen3) = tailExpToExp env' gen2 body (CVAR k')
    in (CAPP k (ULAMBDA x' k' body'), gen3)
  LAPP e1 e2 ->
    expToExp env gen e1 (\gen1 f' ->
      expToExp env gen1 e2 (\gen2 arg' ->
        (UAPP f' arg' k, gen2)))
  LPRIM p es -> expsToVals env gen es (\gen1 args -> (PAPP p args k, gen1))
  LIF e1 e2 e3 ->
    expToExp env gen e1 (\gen1 b' ->
      let (e2', gen2) = tailExpToExp env gen1 e2 k
          (e3', gen3) = tailExpToExp env gen2 e3 k
      in (CIF b' e2' e3', gen3))

expToExp :: Env -> Gen -> Lam -> (Gen -> UVal -> (CExp, Gen)) -> (CExp, Gen)
expToExp env gen e k = case e of
  LVAR x -> k gen (findVar env x)
  LLAMBDA x body ->
    let (x', env', gen1) = bindVar env gen x
        (k', gen2) = newCVar gen1 "k"
        (body', gen3) = tailExpToExp env' gen2 body (CVAR k')
    in k gen3 (ULAMBDA x' k' body')
  LAPP e1 e2 ->
    let (res', gen1) = newUVar gen "res"
    in expToExp env gen1 e1 (\gen2 f' ->
         expToExp env gen2 e2 (\gen3 arg' ->
           let (ct, gen4) = k gen3 (UVAR res')
           in (UAPP f' arg' (CLAMBDA res' ct), gen4)))
  LPRIM p es -> expsToVals env gen es (\gen1 args ->
    let (res', gen2) = newUVar gen1 "res"
        (ct, gen3) = k gen2 (UVAR res')
    in (PAPP p args (CLAMBDA res' ct), gen3))
  LIF e1 e2 e3 ->
    let (k', gen1) = newCVar gen "join"
        (a', gen2) = newUVar gen1 "a"
        (ct, gen3) = k gen2 (UVAR a')
        (e', gen4) = expToExp env gen3 e1 (\gen5 b' ->
          let (e2', gen6) = tailExpToExp env gen5 e2 (CVAR k')
              (e3', gen7) = tailExpToExp env gen6 e3 (CVAR k')
          in (CIF b' e2' e3', gen7))
    in (LETC k' (CLAMBDA a' ct) e', gen4)

expsToVals :: Env -> Gen -> [Lam] -> (Gen -> [UVal] -> (CExp, Gen)) -> (CExp, Gen)
expsToVals env gen0 es0 k = go gen0 es0 []
  where go gen [] uvs = k gen (reverse uvs)
        go gen (e : es) uvs = expToExp env gen e (\gen1 uv -> go gen1 es (uv : uvs))

transform :: Lam -> (CExp, Gen)
transform e = tailExpToExp emptyEnv newGen e HALT

x1, x2, x3, x4, x5, x6 :: Var
x1 = V "a" 1
x2 = V "b" 2
x3 = V "c" 3
x4 = V "d" 4
x5 = V "e" 5
x6 = V "f" 6

e1 :: Lam
e1 = LLAMBDA x3
       (LIF (LAPP (LLAMBDA x1 (LLAMBDA x2 (LPRIM ADD [LIF (LVAR x1) (LVAR x1) (LVAR x2),
                                                  LPRIM ADD [LVAR x1, LVAR x2]])))
                  (LLAMBDA x4 (LVAR x4)))
            (LAPP (LLAMBDA x5 (LVAR x5)) (LLAMBDA x6 (LVAR x3)))
            (LPRIM DIV [LVAR x3, LVAR x3]))

expected :: CExp
expected =
  CAPP HALT
    (ULAMBDA (V "c" 0) (CV "k" 0)
      (UAPP
        (ULAMBDA (V "a" 2) (CV "k" 1)
          (CAPP (CVAR (CV "k" 1))
            (ULAMBDA (V "b" 3) (CV "k" 2)
              (LETC (CV "join" 3)
                (CLAMBDA (V "a" 4)
                  (PAPP ADD [UVAR (V "a" 2), UVAR (V "b" 3)]
                    (CLAMBDA (V "res" 5)
                      (PAPP ADD [UVAR (V "a" 4), UVAR (V "res" 5)] (CVAR (CV "k" 2))))))
                (CIF (UVAR (V "a" 2))
                  (CAPP (CVAR (CV "join" 3)) (UVAR (V "a" 2)))
                  (CAPP (CVAR (CV "join" 3)) (UVAR (V "b" 3))))))))
        (ULAMBDA (V "d" 6) (CV "k" 4) (CAPP (CVAR (CV "k" 4)) (UVAR (V "d" 6))))
        (CLAMBDA (V "res" 1)
          (CIF (UVAR (V "res" 1))
            (UAPP (ULAMBDA (V "e" 7) (CV "k" 5) (CAPP (CVAR (CV "k" 5)) (UVAR (V "e" 7))))
                  (ULAMBDA (V "f" 8) (CV "k" 6) (CAPP (CVAR (CV "k" 6)) (UVAR (V "c" 0))))
                  (CVAR (CV "k" 0)))
            (PAPP DIV [UVAR (V "c" 0), UVAR (V "c" 0)] (CVAR (CV "k" 0)))))))

correctResult :: (CExp, Gen) -> Bool
correctResult (result, gen) = result == expected && gen == Gen (VGen 9) (CGen 7)

main :: IO ()
main = do
  n <- iterations 1000000
  let result = loopLast n (\i -> transform (vary i e1 e1))
  putStrLn (if correctResult result then "pass" else "fail")
