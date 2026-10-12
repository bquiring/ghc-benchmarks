-- knuth-bendix.sml (3cps test/programs/llvm-sml; John Reppy, the SML/NJ
-- benchmark, originally in Caml): Knuth-Bendix completion of the axioms of
-- a group, repeated.
--
-- The SML raises Fail for failure inside pure code (matching, unification,
-- rewriting, lookups) and handles it; here those functions return Maybe.
-- The completion prints as it goes (new and deleted rules), so it runs in
-- IO; each round's lines go to a buffer, and one round's lines are printed
-- (the SML prints every round's, after a "round n" header).
module Main (main) where

import Control.Monad (forM_)
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Repeat

itList :: (a -> b -> a) -> a -> [b] -> a
itList f a0 bs0 = itRec a0 bs0
  where itRec a [] = a
        itRec a (b : l) = itRec (f a b) l

-- it_list2 with a function that can fail
itList2 :: (a -> (b, c) -> Maybe a) -> a -> [b] -> [c] -> Maybe a
itList2 f a0 xs0 ys0 = itRec a0 xs0 ys0
  where itRec a [] [] = Just a
        itRec a (a1 : l1) (a2 : l2) = f a (a1, a2) >>= \a' -> itRec a' l1 l2
        itRec _ _ _ = Nothing   -- failwith "it_list2"

exists :: (a -> Bool) -> [a] -> Bool
exists p = go
  where go [] = False
        go (a : l) = p a || go l

forAll :: (a -> Bool) -> [a] -> Bool
forAll p = go
  where go [] = True
        go (a : l) = p a && go l

-- try_find: the first success
tryFind :: (a -> Maybe b) -> [a] -> Maybe b
tryFind f = go
  where go [] = Nothing
        go (a : l) = case f a of { Nothing -> go l; r -> r }

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p = partRec
  where partRec [] = ([], [])
        partRec (a : l) = let (pos, neg) = partRec l
                          in if p a then (a : pos, neg) else (pos, a : neg)

mem :: String -> [String] -> Bool
mem a = go
  where go [] = False
        go (b : l) = a == b || go l

memAssoc :: Int -> [(Int, b)] -> Bool
memAssoc a = go
  where go [] = False
        go ((b, _) : l) = a == b || go l

-- assoc: Nothing is the SML's failwith "find"
assoc :: Int -> [(Int, b)] -> Maybe b
assoc a = go
  where go [] = Nothing
        go ((b, d) : l) = if a == b then Just d else go l

data Term = Var Int | Term String [Term]

sameTerm :: Term -> Term -> Bool
sameTerm (Var n) (Var m) = n == m
sameTerm (Term f args1) (Term g args2) = f == g && sameTerms args1 args2
  where sameTerms [] [] = True
        sameTerms (t1 : ts1) (t2 : ts2) = sameTerm t1 t2 && sameTerms ts1 ts2
        sameTerms _ _ = False
sameTerm _ _ = False

union :: [Int] -> [Int] -> [Int]
union l1 l2 = unionRec l2
  where memI a = go where { go [] = False; go (b : l) = a == b || go l }
        unionRec [] = l1
        unionRec (a : l) = if memI a l1 then unionRec l else a : unionRec l

vars :: Term -> [Int]
vars (Var n) = [n]
vars (Term _ l) = varsOfList l

varsOfList :: [Term] -> [Int]
varsOfList [] = []
varsOfList (t : r) = union (vars t) (varsOfList r)

type Subst = [(Int, Term)]

substitute :: Subst -> Term -> Term
substitute subst = substRec
  where substRec (Term oper sons) = Term oper (map substRec sons)
        substRec t@(Var n) = fromMaybe t (assoc n subst)

change :: (a -> a) -> [a] -> Int -> [a]
change f = changeRec
  where changeRec (h : t) n = if n == 1 then f h : t else h : changeRec t (n - 1)
        changeRec _ _ = error "change"

replace :: Term -> [Int] -> Term -> Term
replace m u0 nn = reprec m u0
  where reprec _ [] = nn
        reprec (Term oper sons) (n : u) = Term oper (change (\p -> reprec p u) sons n)
        reprec _ _ = error "replace"

matching :: Term -> Term -> Maybe Subst
matching term1 term2 = matchRec [] (term1, term2)
  where matchRec subst (Var v, m)
          | memAssoc v subst = case assoc v subst of
              Just t | sameTerm m t -> Just subst
              _ -> Nothing
          | otherwise = Just ((v, m) : subst)
        matchRec subst (Term op1 sons1, Term op2 sons2)
          | op1 == op2 = itList2 matchRec subst sons1 sons2
          | otherwise = Nothing
        matchRec _ _ = Nothing

compsubst :: Subst -> Subst -> Subst
compsubst subst1 subst2 = map (\(v, t) -> (v, substitute subst1 t)) subst2 ++ subst1

occurs :: Int -> Term -> Bool
occurs n = occurRec
  where occurRec (Var m) = m == n
        occurRec (Term _ sons) = exists occurRec sons

unify :: Term -> Term -> Maybe Subst
unify term1@(Var n1) term2
  | sameTerm term1 term2 = Just []
  | occurs n1 term2 = Nothing
  | otherwise = Just [(n1, term2)]
unify term1 (Var n2)
  | occurs n2 term1 = Nothing
  | otherwise = Just [(n2, term1)]
unify (Term op1 sons1) (Term op2 sons2)
  | op1 == op2 = itList2 (\s (t1, t2) -> fmap (\u -> compsubst u s) (unify (substitute s t1) (substitute s t2)))
                         [] sons1 sons2
  | otherwise = Nothing

infixes :: [String]
infixes = ["+", "*"]

prettyTerm :: Term -> ShowS
prettyTerm (Var n) = showString "v" . shows n
prettyTerm (Term oper sons)
  | mem oper infixes = case sons of
      [s1, s2] -> prettyClose s1 . showString oper . prettyClose s2
      _ -> error "pretty_term : infix arity <> 2"
  | otherwise = showString oper . case sons of
      [] -> id
      t : lt -> showString "(" . prettyTerm t
                . foldr (\t' r -> showString "," . prettyTerm t' . r) id lt
                . showString ")"

prettyClose :: Term -> ShowS
prettyClose m@(Term oper _)
  | mem oper infixes = showString "(" . prettyTerm m . showString ")"
prettyClose m = prettyTerm m

type Rule = (Int, (Int, (Term, Term)))

mkRule :: Term -> Term -> (Int, (Term, Term))
mkRule m n =
  let allVars = union (vars m) (vars n)
      (k, subst) = itList (\(i, sigma) v -> (i + 1, (v, Var i) : sigma)) (1, []) allVars
  in (k - 1, (substitute subst m, substitute subst n))

checkRules :: [Rule] -> Int
checkRules = itList (\n (k, _) -> if k == n + 1 then k else error "Rule numbers not in sequence") 0

prettyRule :: Rule -> String
prettyRule (k, (_, (m, n))) = shows k . showString " : " . prettyTerm m . showString " = " . prettyTerm n $ ""

-- Top-level rewriting: with sigma = matching l m, the image of m is sigma(r)
reduce :: Term -> Term -> Term -> Maybe Term
reduce l m r = fmap (\s -> substitute s r) (matching l m)

reducible :: Term -> Term -> Bool
reducible l = redrec
  where redrec m = isJust (matching l m) || case m of
          Term _ sons -> exists redrec sons
          _ -> False

mreduce :: [Rule] -> Term -> Maybe Term
mreduce rules m = tryFind (\(_, (_, (l, r))) -> reduce l m r) rules

mrewrite1 :: [Rule] -> Term -> Maybe Term
mrewrite1 rules = rewrec
  where rewrec m = case mreduce rules m of
          Nothing -> case m of
            Term f sons -> Term f <$> tryrec sons
            _ -> Nothing
          r -> r
        tryrec [] = Nothing
        tryrec (son : rest) = case rewrec son of
          Just son' -> Just (son' : rest)
          Nothing -> (son :) <$> tryrec rest

mrewriteAll :: [Rule] -> Term -> Term
mrewriteAll rules = rewLoop
  where rewLoop m = maybe m rewLoop (mrewrite1 rules m)

data Ordering' = Greater | Equal | NotGE

geOrd, gtOrd, eqOrd :: (a -> Ordering') -> a -> Bool
geOrd order pair = case order pair of { NotGE -> False; _ -> True }
gtOrd order pair = case order pair of { Greater -> True; _ -> False }
eqOrd order pair = case order pair of { Equal -> True; _ -> False }

remEq :: ((a, a) -> Bool) -> a -> [a] -> Maybe [a]
remEq equiv x = remrec
  where remrec [] = Nothing
        remrec (y : l) = if equiv (x, y) then Just l else (y :) <$> remrec l

diffEq :: ((a, a) -> Bool) -> ([a], [a]) -> ([a], [a])
diffEq equiv (x, y) = if length x > length y then diffrec (y, x) else diffrec (x, y)
  where diffrec p@([], _) = p
        diffrec (h : t, y') = case remEq equiv h y' of
          Just y'' -> diffrec (t, y'')
          Nothing -> let (x', y'') = diffrec (t, y') in (h : x', y'')

type Order = (Term, Term) -> Ordering'

multExt :: Order -> (Term, Term) -> Ordering'
multExt order (Term _ sons1, Term _ sons2) = case diffEq (eqOrd order) (sons1, sons2) of
  ([], []) -> Equal
  (l1, l2) -> if forAll (\n -> exists (\m -> eqOrd order (m, n)) l1) l2 then Greater else NotGE
multExt _ _ = error "mult_ext"

lexExt :: Order -> (Term, Term) -> Ordering'
lexExt order (m@(Term _ sons1), n@(Term _ sons2)) = lexrec (sons1, sons2)
  where lexrec ([], []) = Equal
        lexrec ([], _) = NotGE
        lexrec (_, []) = Greater
        lexrec (x1 : l1, x2 : l2) = case order (x1, x2) of
          Greater -> if forAll (\n' -> gtOrd order (m, n')) l2 then Greater else NotGE
          Equal -> lexrec (l1, l2)
          NotGE -> if exists (\m' -> geOrd order (m', n)) l1 then Greater else NotGE
lexExt _ _ = error "lex_ext"

rpo :: (String -> String -> Ordering') -> (Order -> (Term, Term) -> Ordering') -> Order
rpo opOrder ext = rporec
  where
    rporec (m, n)
      | sameTerm m n = Equal
      | otherwise = case m of
          Var _ -> NotGE
          Term op1 sons1 -> case n of
            Var v -> if occurs v m then Greater else NotGE
            Term op2 sons2 -> case opOrder op1 op2 of
              Greater -> if forAll (\n' -> gtOrd rporec (m, n')) sons2 then Greater else NotGE
              Equal -> ext rporec (m, n)
              NotGE -> if exists (\m' -> geOrd rporec (m', n)) sons1 then Greater else NotGE

-- All (u, sig) such that n/u (&var) unifies with m, with principal unifier sig
super :: Term -> Term -> [([Int], Subst)]
super m = suprec
  where suprec nn@(Term _ sons) =
          let collate (pairs, k) son = (pairs ++ map (\(u, sigma) -> (k : u, sigma)) (suprec son), k + 1)
              insides = fst (itList collate ([], 1) sons)
          in case unify m nn of
               Just s -> ([], s) : insides
               Nothing -> insides
        suprec _ = []

superStrict :: Term -> Term -> [([Int], Subst)]
superStrict m (Term _ sons) =
  let collate (pairs, k) son = (pairs ++ map (\(u, sigma) -> (k : u, sigma)) (super m son), k + 1)
  in fst (itList collate ([], 1) sons)
superStrict _ _ = []

criticalPairs :: (Term, Term) -> (Term, Term) -> [(Term, Term)]
criticalPairs (l1, r1) (l2, r2) =
  map (\(u, sigma) -> (substitute sigma (replace l2 u r1), substitute sigma r2)) (super l1 l2)

strictCriticalPairs :: (Term, Term) -> (Term, Term) -> [(Term, Term)]
strictCriticalPairs (l1, r1) (l2, r2) =
  map (\(u, sigma) -> (substitute sigma (replace l2 u r1), substitute sigma r2)) (superStrict l1 l2)

mutualCriticalPairs :: (Term, Term) -> (Term, Term) -> [(Term, Term)]
mutualCriticalPairs eq1 eq2 = strictCriticalPairs eq1 eq2 ++ criticalPairs eq2 eq1

rename :: Int -> (Term, Term) -> (Term, Term)
rename n (t1, t2) = (renRec t1, renRec t2)
  where renRec (Var k) = Var (k + n)
        renRec (Term oper sons) = Term oper (map renRec sons)

kbCompletion :: (String -> IO ()) -> ((Term, Term) -> Bool) -> Int -> [Rule] -> [(Term, Term)]
             -> (Int, Int) -> [(Term, Term)] -> IO [Rule]
kbCompletion say greater = kbrec
  where
    kbrec n rules = process
      where
        normalForm = mrewriteAll rules
        getRule k = assoc k rules
        process failures kl eqs = processf kl eqs
          where
            processf (k, l) = processkl
              where
                processkl [] =
                  if k < l then nextCriticals (k + 1, l) else
                  if l < n then nextCriticals (1, l + 1) else
                  case failures of
                    [] -> return rules   -- successful completion
                    _ -> do say "Non-orientable equations :"
                            mapM_ (\(m, nn) -> say (prettyTerm m . showString " = " . prettyTerm nn $ "")) failures
                            error "kb_completion"
                processkl ((m, nn) : eqs') = do
                  let m' = normalForm m
                      n' = normalForm nn
                      enterRule (left, right) = do
                        let newRule = (n + 1, mkRule left right)
                        say (prettyRule newRule)
                        let leftReducible (_, (_, (l', _))) = reducible left l'
                            (redl, irredl) = partition leftReducible rules
                        mapM_ (\(k', _) -> say ("Rule " ++ show k' ++ " deleted")) redl
                        let rightReduce (m'', (_, (l', r))) = (m'', mkRule l' (mrewriteAll (newRule : rules) r))
                            irreds = map rightReduce irredl
                            eqs'' = map (\(_, (_, pair)) -> pair) redl
                        kbrec (n + 1) (newRule : irreds) [] (k, l) (eqs' ++ eqs'' ++ failures)
                  if sameTerm m' n' then processkl eqs'
                  else if greater (m', n') then enterRule (m', n')
                  else if greater (n', m') then enterRule (n', m')
                  else process ((m', n') : failures) (k, l) eqs'
            nextCriticals (k, l) = case getRule l of
              Nothing -> nextCriticals (1, l + 1)           -- rule l deleted
              Just (v, el)
                | k == l -> processf (k, l) (strictCriticalPairs el (rename v el))
                | otherwise -> case getRule k of
                    Nothing -> nextCriticals (k + 1, l)     -- rule k deleted
                    Just (_, ek) -> processf (k, l) (mutualCriticalPairs el (rename v ek))

kbComplete :: (String -> IO ()) -> ((Term, Term) -> Bool) -> [Rule] -> [Rule] -> IO [Rule]
kbComplete say greater completeRules rules = do
  let n = checkRules completeRules
      eqs = map (\(_, (_, pair)) -> pair) rules
  completed <- kbCompletion say greater n completeRules [] (n, n) eqs
  say "Canonical set found :"
  mapM_ (say . prettyRule) (reverse completed)
  return completed

geomRules :: [Rule]
geomRules =
  [ (1, (1, (Term "*" [Term "U" [], Var 1], Var 1)))
  , (2, (1, (Term "*" [Term "I" [Var 1], Var 1], Term "U" [])))
  , (3, (3, (Term "*" [Term "*" [Var 1, Var 2], Var 3], Term "*" [Var 1, Term "*" [Var 2, Var 3]]))) ]

groupRank :: String -> Int
groupRank s
  | s == "U" = 0
  | s == "*" = 1
  | s == "I" = 2
  | s == "B" = 3
  | s == "C" = 4
  | s == "A" = 5
  | otherwise = -1

groupPrecedence :: String -> String -> Ordering'
groupPrecedence op1 op2 =
  let r1 = groupRank op1
      r2 = groupRank op2
  in if r1 == r2 then Equal else if r1 > r2 then Greater else NotGE

groupOrder :: Order
groupOrder = rpo groupPrecedence lexExt

greater :: (Term, Term) -> Bool
greater pair = case groupOrder pair of { Greater -> True; _ -> False }

sameRule :: Rule -> Rule -> Bool
sameRule (n1, (_, (t11, t12))) (n2, (_, (t21, t22))) = n1 == n2 && sameTerm t11 t21 && sameTerm t12 t22

correctRules :: [Rule] -> Bool
correctRules rules = case reverse rules of
  [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10] ->
    sameRule r1 (1, (0, (Term "*" [Term "U" [], Var 1], Var 1))) &&
    sameRule r2 (2, (0, (Term "*" [Term "I" [Var 1], Var 1], Term "U" []))) &&
    sameRule r3 (3, (0, (Term "*" [Term "*" [Var 3, Var 2], Var 1], Term "*" [Var 3, Term "*" [Var 2, Var 1]]))) &&
    sameRule r4 (4, (0, (Term "*" [Term "I" [Var 2], Term "*" [Var 2, Var 1]], Var 1))) &&
    sameRule r5 (9, (0, (Term "*" [Var 1, Term "U" []], Var 1))) &&
    sameRule r6 (14, (0, (Term "*" [Var 1, Term "I" [Var 1]], Term "U" []))) &&
    sameRule r7 (15, (0, (Term "*" [Var 2, Term "*" [Term "I" [Var 2], Var 1]], Var 1))) &&
    sameRule r8 (16, (0, (Term "I" [Term "U" []], Term "U" []))) &&
    sameRule r9 (17, (0, (Term "I" [Term "I" [Var 1]], Var 1))) &&
    sameRule r10 (41, (0, (Term "I" [Term "*" [Var 2, Var 1]], Term "*" [Term "I" [Var 1], Term "I" [Var 2]])))
  _ -> False

-- One round: its result and the lines it printed
doit :: Int -> IO ([Rule], [String])
doit i = do
  buf <- newIORef []
  let say s = modifyIORef' buf (s :)
  rules <- kbComplete say greater [] (vary i geomRules [])
  out <- readIORef buf
  return (rules, reverse out)

main :: IO ()
main = do
  n <- iterations 500
  (rules1, out1) <- doit 1
  sameAll <- allM (\i -> do { (_, out) <- doit i; return (out == out1) }) [2 .. n]
  forM_ (if sameAll then out1 else ["fail: rounds differ"]) putStrLn
  putStrLn (if correctRules rules1 then "pass" else "fail")
  where allM _ [] = return True
        allM p (x : xs) = p x >>= \b -> if b then allM p xs else return False
