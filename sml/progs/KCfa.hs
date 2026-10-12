-- k-cfa.sml (3cps test/programs/llvm-sml; John Reppy, after Matthew Might's
-- Scheme/Racket k-CFA): 1-CFA of a small CPS program as a small-step
-- abstract interpreter, with red-black-tree maps and sets (after the SML/NJ
-- Library), 10001 times.
--
-- The SML counts a map's items in a ref while inserting; here insertion
-- returns whether the key was new.
module Main (main) where

import Repeat

-- Labels, variables, time
type Label = Int
type Var = String

-- List.collate
collate :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
collate _ [] [] = EQ
collate _ [] _ = LT
collate _ _ [] = GT
collate cmp (x : xs) (y : ys) = case cmp x y of
  EQ -> collate cmp xs ys
  o -> o

-- ListPair.foldl: stops at the shorter list
foldl2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldl2 f ini l1 l2 = go l1 l2 ini
  where go (x : xs) (y : ys) acc = go xs ys (f x y acc)
        go _ _ acc = acc

-- ListPair.allEq
allEq :: (a -> b -> Bool) -> [a] -> [b] -> Bool
allEq _ [] [] = True
allEq p (x : xs) (y : ys) = p x y && allEq p xs ys
allEq _ _ _ = False

-- Red-black maps (Okasaki's insertion)
data Color = R | B

data Tree k a = E | T Color (Tree k a) k a (Tree k a)

data Map k a = MAP Int (Tree k a)

mapEmpty :: Map k a
mapEmpty = MAP 0 E

-- insertWith: comb old new when the key is already present
mapInsertWith :: (k -> k -> Ordering) -> (a -> a -> a) -> Map k a -> k -> a -> Map k a
mapInsertWith compare' comb (MAP nItems m) xk x =
  case ins m of
    (T _ a yk y b, new) -> MAP (if new then nItems + 1 else nItems) (T B a yk y b)
    (E, _) -> error "insert"
  where
    ins E = (T R E xk x E, True)
    ins (T color a yk y b) = case compare' xk yk of
      LT -> case a of
        T R c zk z d -> case compare' xk zk of
          LT -> case ins c of
            (T R e wk w f, n) -> (T R (T B e wk w f) zk z (T B d yk y b), n)
            (c', n) -> (T B (T R c' zk z d) yk y b, n)
          EQ -> (T color (T R c xk (comb z x) d) yk y b, False)
          GT -> case ins d of
            (T R e wk w f, n) -> (T R (T B c zk z e) wk w (T B f yk y b), n)
            (d', n) -> (T B (T R c zk z d') yk y b, n)
        _ -> let (a', n) = ins a in (T B a' yk y b, n)
      EQ -> (T color a xk (comb y x) b, False)
      GT -> case b of
        T R c zk z d -> case compare' xk zk of
          LT -> case ins c of
            (T R e wk w f, n) -> (T R (T B a yk y e) wk w (T B f zk z d), n)
            (c', n) -> (T B a yk y (T R c' zk z d), n)
          EQ -> (T color a yk y (T R c xk (comb z x) d), False)
          GT -> case ins d of
            (T R e wk w f, n) -> (T R (T B a yk y c) zk z (T B e wk w f), n)
            (d', n) -> (T B a yk y (T R c zk z d'), n)
        _ -> let (b', n) = ins b in (T B a yk y b', n)

-- The Var.Map insert: replaces the value
mapInsert :: (k -> k -> Ordering) -> Map k a -> k -> a -> Map k a
mapInsert compare' = mapInsertWith compare' (\_ new -> new)

mapFind :: (k -> k -> Ordering) -> Map k a -> k -> Maybe a
mapFind compare' (MAP _ t) k = find' t
  where find' E = Nothing
        find' (T _ a yk y b) = case compare' k yk of
          LT -> find' a
          EQ -> Just y
          GT -> find' b

-- Walking a tree in order, keeping a stack of the parents still to visit
next :: [Tree k a] -> (Tree k a, [Tree k a])
next (t@(T _ _ _ _ b) : rest) = (t, left b rest)
next _ = (E, [])

left :: Tree k a -> [Tree k a] -> [Tree k a]
left E rest = rest
left t@(T _ a _ _ _) rest = left a (t : rest)

start :: Tree k a -> [Tree k a]
start m = left m []

mapCollate :: (k -> k -> Ordering) -> (a -> a -> Ordering) -> Map k a -> Map k a -> Ordering
mapCollate compare' cmpRng (MAP _ m1) (MAP _ m2) = cmp (start m1) (start m2)
  where cmp t1 t2 = case (next t1, next t2) of
          ((E, _), (E, _)) -> EQ
          ((E, _), _) -> LT
          (_, (E, _)) -> GT
          ((T _ _ xk x _, r1), (T _ _ yk y _, r2)) -> case compare' xk yk of
            EQ -> case cmpRng x y of
              EQ -> cmp r1 r2
              o -> o
            o -> o

mapFoldli :: (k -> a -> b -> b) -> b -> Map k a -> b
mapFoldli f ini (MAP _ m) = foldf m ini
  where foldf E accum = accum
        foldf (T _ a xk x b) accum = foldf b (f xk x (foldf a accum))

-- Constructing red-black trees in linear time from increasing sequences
-- (after R. Hinze); a digit's elements are ordered largest first
data Digit k a = ZERO | ONE k a (Tree k a) (Digit k a) | TWO k a (Tree k a) k a (Tree k a) (Digit k a)

addItem :: k -> a -> Digit k a -> Digit k a
addItem ak a l = incr ak a E l
  where incr k1 a1 t1 ZERO = ONE k1 a1 t1 ZERO
        incr k1 a1 t1 (ONE k2 a2 t2 r) = TWO k1 a1 t1 k2 a2 t2 r
        incr k1 a1 t1 (TWO k2 a2 t2 k3 a3 t3 r) = ONE k1 a1 t1 (incr k2 a2 (T B t3 k3 a3 t2) r)

linkAll :: Digit k a -> Tree k a
linkAll t = link E t
  where link t' ZERO = t'
        link t1 (ONE ak a t2 r) = link (T B t2 ak a t1) r
        link t' (TWO ak1 a1 t1 ak2 a2 t2 r) = link (T B (T R t2 ak2 a2 t1) ak1 a1 t') r

-- A map whose domain is the union of both domains, merging common keys
mapUnionWith :: (k -> k -> Ordering) -> (a -> a -> a) -> Map k a -> Map k a -> Map k a
mapUnionWith compare' mergeFn (MAP _ m1) (MAP _ m2) =
  let (n, result) = union (start m1) (start m2) 0 ZERO in MAP n (linkAll result)
  where
    ins (E, _) n result = (n, result)
    ins (T _ _ xk x _, r) n result = ins (next r) (n + 1) (addItem xk x result)
    union t1 t2 n result = case (next t1, next t2) of
      ((E, _), (E, _)) -> (n, result)
      ((E, _), t2') -> ins t2' n result
      (t1', (E, _)) -> ins t1' n result
      ((T _ _ xk x _, r1), (T _ _ yk y _, r2)) -> case compare' xk yk of
        LT -> union r1 t2 (n + 1) (addItem xk x result)
        EQ -> union r1 r2 (n + 1) (addItem xk (mergeFn x y) result)
        GT -> union t1 r2 (n + 1) (addItem yk y result)

-- Sets: maps to ()
type Set a = Map a ()

setSingleton :: a -> Set a
setSingleton x = MAP 1 (T B E x () E)

setCompare :: (a -> a -> Ordering) -> Set a -> Set a -> Ordering
setCompare cmp = mapCollate cmp (\_ _ -> EQ)

setUnion :: (a -> a -> Ordering) -> Set a -> Set a -> Set a
setUnion cmp = mapUnionWith cmp (\x _ -> x)

setFoldl :: (a -> b -> b) -> b -> Set a -> b
setFoldl f = mapFoldli (\x _ acc -> f x acc)

-- Syntax
data Exp = VAR Label Var | LAMBDA Lambda
data Call = CALL Label Exp [Exp] | RET Label Var
type Lambda = (Label, [Var], Call)

compareLambda :: Lambda -> Lambda -> Ordering
compareLambda (lab1, _, _) (lab2, _, _) = compare lab1 lab2

compareExp :: Exp -> Exp -> Ordering
compareExp (VAR _ x) (VAR _ y) = compare x y
compareExp (VAR _ _) _ = LT
compareExp _ (VAR _ _) = GT
compareExp (LAMBDA lam1) (LAMBDA lam2) = compareLambda lam1 lam2

sameCall :: Call -> Call -> Bool
sameCall (CALL l1 _ _) (CALL l2 _ _) = l1 == l2
sameCall (RET l1 _) (RET l2 _) = l1 == l2
sameCall _ _ = False

-- Time: the last k call sites
type Time = [Label]

timeCompare :: Time -> Time -> Ordering
timeCompare = collate compare

-- Bindings are the only addresses
data Bind = Bnd Var Time
type Addr = Bind

addrCompare :: Addr -> Addr -> Ordering
addrCompare (Bnd x t1) (Bnd y t2) = case compare x y of
  EQ -> timeCompare t1 t2
  o -> o

type BEnv = Map Var Addr

benvCompare :: BEnv -> BEnv -> Ordering
benvCompare = mapCollate compare addrCompare

benvLookup :: BEnv -> Var -> Addr
benvLookup benv x = case mapFind compare benv x of
  Just adr -> adr
  Nothing -> error "lookup"

benvExtend' :: BEnv -> [Var] -> [Addr] -> BEnv
benvExtend' benv xs adrs = foldl2 (\x adr b -> mapInsert compare b x adr) benv xs adrs

-- A closure pairs a lambda with the environment of its free variables
data Closure = Clos Lambda BEnv

closureCompare :: Closure -> Closure -> Ordering
closureCompare (Clos lam1 benv1) (Clos lam2 benv2) = case compareLambda lam1 lam2 of
  EQ -> benvCompare benv1 benv2
  o -> o

-- Abstract values: sets of closures
type D = Set Closure

dJoin :: D -> D -> D
dJoin = setUnion closureCompare

dCompare :: D -> D -> Ordering
dCompare = setCompare closureCompare

type Store = Map Addr D

storeLookup :: Store -> Addr -> D
storeLookup st adr = case mapFind addrCompare st adr of
  Just dv -> dv
  Nothing -> error "store lookup"

storeUpdate :: Store -> Addr -> D -> Store
storeUpdate = mapInsertWith addrCompare dJoin

storeUpdate' :: Store -> [Addr] -> [D] -> Store
storeUpdate' st adrs dvs = foldl2 (\adr dv s -> storeUpdate s adr dv) st adrs dvs

storeJoin :: Store -> Store -> Store
storeJoin = mapUnionWith addrCompare dJoin

storeCompare :: Store -> Store -> Ordering
storeCompare = mapCollate addrCompare dCompare

data State = State Call BEnv Store Time

storeOf :: State -> Store
storeOf (State _ _ st _) = st

sameState :: State -> State -> Bool
sameState (State call1 bev1 st1 t1) (State call2 bev2 st2 t2) =
  allEq (==) t1 t2 && sameCall call1 call2
  && (case benvCompare bev1 bev2 of { EQ -> True; _ -> False })
  && (case storeCompare st1 st2 of { EQ -> True; _ -> False })

initState :: Call -> State
initState call = State call mapEmpty mapEmpty []

-- k = 1
tick :: Call -> Time -> Time
tick (CALL lab _ _) _ = [lab]
tick _ _ = error "tick"

alloc :: Time -> Var -> Addr
alloc t x = Bnd x t

atomEval :: BEnv -> Store -> Exp -> D
atomEval benv store (VAR _ x) = storeLookup store (benvLookup benv x)
atomEval benv _ (LAMBDA lam) = setSingleton (Clos lam benv)

nextStates :: State -> [State]
nextStates (State call@(CALL _ f args) benv store time) =
  let time' = tick call time
      procs = atomEval benv store f
      params = map (atomEval benv store) args
      apply (Clos (_, formals, call') benv') states =
        let bindings = map (alloc time') formals
            benv'' = benvExtend' benv' formals bindings
            store' = storeUpdate' store bindings params
        in State call' benv'' store' time' : states
  in setFoldl apply [] procs
nextStates _ = []

member :: [State] -> State -> Bool
member sts st = any (\st' -> sameState st st') sts

insertExp :: Exp -> [Exp] -> [Exp]
insertExp e [] = [e]
insertExp e (e' : es) = case compareExp e e' of
  LT -> e' : insertExp e es
  GT -> e : e' : es
  EQ -> e' : es

type MonoStore = [(Var, [Exp])]

monoValues :: D -> [Exp]
monoValues = setFoldl (\(Clos lam _) exps -> LAMBDA lam : exps) []

monoStoreUpdate :: MonoStore -> Var -> Exp -> MonoStore
monoStoreUpdate [] x e = [(x, [e])]
monoStoreUpdate ((y, exps) : r) x e = case compare x y of
  LT -> (y, exps) : monoStoreUpdate r x e
  GT -> (x, [e]) : (y, exps) : r
  EQ -> (y, insertExp e exps) : r

monoStoreUpdate' :: MonoStore -> Var -> [Exp] -> MonoStore
monoStoreUpdate' mst x exps = foldl (\m e -> monoStoreUpdate m x e) mst exps

explore :: [State] -> [State] -> [State]
explore seen [] = seen
explore seen (st : sts)
  | member seen st = explore seen sts
  | otherwise = explore (st : seen) (nextStates st ++ sts)

summarize :: [State] -> Store
summarize = foldl (\s state -> storeJoin (storeOf state) s) mapEmpty

monovariantStore :: Store -> MonoStore
monovariantStore = mapFoldli (\(Bnd x _) d mst -> monoStoreUpdate' mst x (monoValues d)) []

-- The standard example:
--   (let* ((id (lambda (x) x))
--          (a  (id (lambda (z) (halt z))))
--          (b  (id (lambda (y) (halt y)))))
--     (halt b))
-- The SML numbers the nodes from a counter as it builds them (arguments
-- left to right, a node after its children); these are those labels.
stdExample :: Call
stdExample =
  CALL 18 (LAMBDA (17, ["id"],
             CALL 16 (VAR 6 "id")
               [ LAMBDA (8, ["z"], RET 7 "z")
               , LAMBDA (15, ["a"],
                   CALL 14 (VAR 9 "id")
                     [ LAMBDA (11, ["y"], RET 10 "y")
                     , LAMBDA (13, ["b"], RET 12 "b") ]) ]))
          [LAMBDA (5, ["x", "k"], CALL 4 (VAR 2 "k") [VAR 3 "x"])]

analyse :: Call -> MonoStore
analyse prog = monovariantStore (summarize (explore [] [initState prog]))

check :: MonoStore -> Bool
check r = case r of
  [ (_, [LAMBDA (11, [_], RET 10 _), LAMBDA (8, [_], RET 7 _)])
    , (_, [LAMBDA (15, [_], CALL 14 (VAR 9 _) [LAMBDA _, LAMBDA _]), LAMBDA (13, [_], RET 12 _)])
    , (_, [LAMBDA (5, [_, _], CALL 4 (VAR 2 _) [VAR _ _])])
    , (_, [LAMBDA (11, [_], RET 10 _)])
    , (_, [LAMBDA (8, [_], RET 7 _)]) ] -> True
  _ -> False

-- Forces a result completely: the check reads only its labels
force :: MonoStore -> Int
force = sum . map (\(x, es) -> length x + sum (map expSize es))
  where expSize (VAR l x) = l + length x
        expSize (LAMBDA (l, xs, c)) = l + length xs + callSize c
        callSize (CALL l f as) = l + expSize f + sum (map expSize as)
        callSize (RET l x) = l + length x

main :: IO ()
main = do
  n <- iterations 250000
  let go i | i >= n = analyse (vary i stdExample stdExample)
           | otherwise = force (analyse (vary i stdExample stdExample)) `seq` go (i + 1)
  putStrLn (if check (go 1) then "pass" else "fail")
