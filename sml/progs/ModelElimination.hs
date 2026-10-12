-- model-elimination.sml (MLton benchmarks, 3cps test/programs/from-mlton;
-- Joe Hurd's Metis prover, 2002): first-order theorem proving by model
-- elimination (MESON) on 14 problems, with Hurd's library (balanced binary
-- maps, a parser for formulas, substitutions, matching and unification,
-- term and literal discrimination nets, subsumption, an LCF-style kernel,
-- clausal normal form, a unit cache) and a solver scheduler that gives
-- each solver time slices of 1/3 s of CPU time.  The searches run in IO:
-- refs, timers and ERR exceptions used for backtracking (Haskell
-- exceptions here) are woven through them; the logic is pure, with ERR
-- where it is caught as Maybe.  Main runs only MESON (the benchmark's
-- "pure" setting), so resolution, delta, the Knuth-Bendix order, heaps,
-- queues, multisets and the pretty-printer (used only in error messages)
-- are not translated, and Problem1 holds only the problems Main uses.
-- Output: what the SML prints, one round's, without the lines that contain
-- CPU times (MESON's progress lines and the timings of each problem), so
-- the output is the same from run to run; tools/check-all.sh drops the
-- same lines from MLton's output.
module Main (main) where

import Control.Exception
import Control.Monad (forM_, unless, when)
import Data.Char (isAlpha, isDigit, isLower, isSpace, isUpper, toLower)
import Data.IORef
import Data.List (isPrefixOf, partition)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, mapMaybe)
import GHC.Clock (getMonotonicTime)
import Numeric (showFFloat)
import System.CPUTime (getCPUTime)
import System.IO.Unsafe (unsafePerformIO)
import Repeat

------------------------------------------------------------------------
-- Useful

-- ERR_EXN: the exception searches backtrack on
data ErrExn = ErrExn String String deriving Show
instance Exception ErrExn

throwErr :: String -> String -> IO a
throwErr f s = throwIO (ErrExn f s)

orThrow :: String -> Maybe a -> IO a
orThrow f = maybe (throwErr f "") return

catchErr :: IO a -> IO a -> IO a
catchErr act h = act `catch` \(ErrExn _ _) -> h

bug :: String -> String -> a
bug f s = error ("BUG in function " ++ f ++ ": " ++ s)

smlInt :: Int -> String
smlInt n = if n < 0 then '~' : show (negate n) else show n

-- What the program prints, this round
outBuf :: IORef [String]
outBuf = unsafePerformIO (newIORef [])
{-# NOINLINE outBuf #-}

prn :: String -> IO ()
prn s = modifyIORef outBuf (s :)

tracing :: Int
tracing = 1

traces :: IORef [(String, Int -> Int)]
traces = unsafePerformIO (newIORef [("Meson1", const 1), ("Solver1", const 1), ("Subsume1", const 1)])
{-# NOINLINE traces #-}

trace' :: String -> String -> Int -> IO ()
trace' m s l = do
  ts <- readIORef traces
  let maxT = 10
      query = case lookup m ts of { Nothing -> maxT; Just alignment -> alignment l }
  when (0 < tracing && (maxT <= tracing || query <= tracing)) (prn s)

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = nTimes (n - 1) f (f x)

cartwith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartwith f xs ys = let xs' = reverse xs in aux xs' [] xs' (reverse ys)
  where aux _ res _ [] = res
        aux xsCopy res [] (_ : yt) = aux xsCopy res xsCopy yt
        aux xsCopy res (x : xt) ys'@(y : _) = aux xsCopy (f x y : res) xt ys'

zipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith f xs0 ys0 = reverse (z [] xs0 ys0)
  where z l [] [] = l
        z l (x : xs) (y : ys) = z (f x y : l) xs ys
        z _ _ _ = error "zipwith: lists different lengths"

split :: [a] -> Int -> ([a], [a])
split l0 n0 = aux [] l0 n0
  where aux res l 0 = (reverse res, l)
        aux _ [] _ = error "Subscript"
        aux res (h : t) n = aux (h : res) t (n - 1)

updateNth :: (a -> a) -> Int -> [a] -> [a]
updateNth f n l = case split l n of
  (a, h : t) -> a ++ (f h : t)
  _ -> error "Subscript"

mem :: Eq a => a -> [a] -> Bool
mem x = any (== x)

insertL :: Eq a => a -> [a] -> [a]
insertL x s = if mem x s then s else x : s

setify :: Eq a => [a] -> [a]
setify = foldl (\x v -> if mem v x then x else v : x) []

unionL :: Eq a => [a] -> [a] -> [a]
unionL s t = foldl (\x v -> if mem v x then x else v : x) s t

intersectL :: Eq a => [a] -> [a] -> [a]
intersectL s t = foldl (\x v -> if mem v t then v : x else x) [] s

subtractL :: Eq a => [a] -> [a] -> [a]
subtractL s t = foldl (\x v -> if mem v t then x else v : x) [] s

minBy :: (a -> a -> Bool) -> [a] -> a
minBy _ [] = error "min: empty list"
minBy f (h : t) = minAcc h t
  where minAcc best [] = best
        minAcc best (x : xs) = minAcc (if f best x then best else x) xs

mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy f = mrg []
  where mrg res [] ys = foldl (flip (:)) ys res
        mrg res xs [] = foldl (flip (:)) xs res
        mrg res xs@(x : xt) ys@(y : yt) = if f x y then mrg (x : res) xt ys else mrg (y : res) xs yt

sortBy' :: (a -> a -> Bool) -> [a] -> [a]
sortBy' f = srt
  where srt [] = []
        srt l@[_] = l
        srt l = let half = length l `div` 2 in mergeBy f (srt (take half l)) (srt (drop half l))

interval :: Int -> Int -> [Int]
interval _ 0 = []
interval m len = m : interval (m + 1) (len - 1)

variant :: String -> [String] -> String
variant x vars = if mem x vars then variant (x ++ "'") vars else x

stringToInt :: String -> Int
stringToInt s = case span isDigit s of
  ("", _) -> error "Option"
  (ds, _) -> read ds

data Tree a b = BRANCH a [Tree a b] | LEAF b

treeFoldr :: (a -> [c] -> c) -> (b -> c) -> Tree a b -> c
treeFoldr _ fl (LEAF l) = fl l
treeFoldr fb fl (BRANCH p s) = fb p (map (treeFoldr fb fl) s)

treePartialFoldl :: (a -> c -> Maybe c) -> (b -> c -> Maybe d) -> c -> Tree a b -> [d]
treePartialFoldl fb fl state0 t0 = fold state0 t0 []
  where fold state (LEAF l) res = case fl l state of { Nothing -> res; Just x -> x : res }
        fold state (BRANCH p ts) res = case fb p state of
          Nothing -> res
          Just s -> foldl (\acc t -> fold s t acc) res ts

generator :: IORef Int
generator = unsafePerformIO (newIORef 0)
{-# NOINLINE generator #-}

newInts :: Int -> IO [Int]
newInts 0 = return []
newInts k = do
  n <- readIORef generator
  writeIORef generator (n + k)
  return (interval n k)

withFlag :: IORef r -> (r -> r) -> (a -> IO b) -> a -> IO b
withFlag r update f x = do
  old <- readIORef r
  writeIORef r (update old)
  y <- f x `onException` writeIORef r old
  writeIORef r old
  return y

------------------------------------------------------------------------
-- Binarymap: weight-balanced trees

data Dict k a = DICT (k -> k -> Ordering) (BTree k a)

data BTree k a = E | T k a Int (BTree k a) (BTree k a)

treeSize :: BTree k a -> Int
treeSize E = 0
treeSize (T _ _ cnt _ _) = cnt

numItems :: Dict k a -> Int
numItems (DICT _ t) = treeSize t

wt :: Int -> Int
wt i = 3 * i

nNode :: k -> a -> BTree k a -> BTree k a -> BTree k a
nNode k v l r = T k v (1 + treeSize l + treeSize r) l r

singleL, singleR, doubleL, doubleR :: k -> a -> BTree k a -> BTree k a -> BTree k a
singleL a av x (T b bv _ y z) = nNode b bv (nNode a av x y) z
singleL _ _ _ _ = error "Match"
singleR b bv (T a av _ x y) z = nNode a av x (nNode b bv y z)
singleR _ _ _ _ = error "Match"
doubleL a av w (T c cv _ (T b bv _ x y) z) = nNode b bv (nNode a av w x) (nNode c cv y z)
doubleL _ _ _ _ = error "Match"
doubleR c cv (T a av _ w (T b bv _ x y)) z = nNode b bv (nNode a av w x) (nNode c cv y z)
doubleR _ _ _ _ = error "Match"

tBal :: k -> a -> BTree k a -> BTree k a -> BTree k a
tBal k v E E = T k v 1 E E
tBal k v E r@(T _ _ _ E E) = T k v 2 E r
tBal k v l@(T _ _ _ E E) E = T k v 2 l E
tBal k v E r@(T _ _ _ (T {}) E) = doubleL k v E r
tBal k v l@(T _ _ _ E (T {})) E = doubleR k v l E
tBal k v E r@(T _ _ _ (T _ _ ln _ _) (T _ _ rn _ _)) = if ln < rn then singleL k v E r else doubleL k v E r
tBal k v l@(T _ _ _ (T _ _ ln _ _) (T _ _ rn _ _)) E = if ln > rn then singleR k v l E else doubleR k v l E
tBal k v E r@(T _ _ _ E _) = singleL k v E r
tBal k v l@(T _ _ _ _ E) E = singleR k v l E
tBal k v l@(T _ _ ln ll lr) r@(T _ _ rn rl rr)
  | rn >= wt ln = if treeSize rl < treeSize rr then singleL k v l r else doubleL k v l r
  | ln >= wt rn = if treeSize lr < treeSize ll then singleR k v l r else doubleR k v l r
  | otherwise = T k v (ln + rn + 1) l r

delete' :: BTree k a -> BTree k a -> BTree k a
delete' E r = r
delete' l E = l
delete' l r = let (mink, minv) = minT r in tBal mink minv l (delmin r)
  where minT (T key value _ E _) = (key, value)
        minT (T _ _ _ left _) = minT left
        minT _ = error "Match"
        delmin (T _ _ _ E right) = right
        delmin (T key value _ left right) = tBal key value (delmin left) right
        delmin _ = error "Match"

mkDict :: (k -> k -> Ordering) -> Dict k a
mkDict cmp = DICT cmp E

dInsert :: Dict k a -> k -> a -> Dict k a
dInsert (DICT cmpKey t) x v = DICT cmpKey (ins t)
  where ins E = T x v 1 E E
        ins (T key value cnt left right) = case cmpKey key x of
          GT -> tBal key value (ins left) right
          LT -> tBal key value left (ins right)
          EQ -> T x v cnt left right

dPeek :: Dict k a -> k -> Maybe a
dPeek (DICT cmpKey t) x = mem' t
  where mem' E = Nothing
        mem' (T key value _ left right) = case cmpKey x key of
          GT -> mem' right
          LT -> mem' left
          EQ -> Just value

-- Nothing: the SML's NotFound
dRemove :: Dict k a -> k -> Maybe (Dict k a, a)
dRemove (DICT cmpKey t) x = (\(t', v) -> (DICT cmpKey t', v)) <$> rmT t
  where rmT E = Nothing
        rmT (T key value _ left right) = case cmpKey key x of
          GT -> (\(left', v) -> (tBal key value left' right, v)) <$> rmT left
          LT -> (\(right', v) -> (tBal key value left right', v)) <$> rmT right
          EQ -> Just (delete' left right, value)

dListItems :: Dict k a -> [(k, a)]
dListItems (DICT _ d) = d2l d []
  where d2l E res = res
        d2l (T key value _ left right) res = d2l left ((key, value) : d2l right res)

dFoldl :: (k -> a -> b -> b) -> b -> Dict k a -> b
dFoldl f initial (DICT _ d) = a d initial
  where a E v = v
        a (T key value _ left right) v = a right (f key value (a left v))

dFoldr :: (k -> a -> b -> b) -> b -> Dict k a -> b
dFoldr f initial (DICT _ d) = a d initial
  where a E v = v
        a (T key value _ left right) v = a left (f key value (a right v))

------------------------------------------------------------------------
-- Parser (pure: Nothing is Noparse)

type Parser t a = [t] -> Maybe (a, [t])

infixr 8 +++
infixr 7 >>>
infixr 6 |||

(+++) :: Parser t a -> Parser t b -> Parser t (a, b)
(p1 +++ p2) input = do
  (r1, rest1) <- p1 input
  (r2, rest2) <- p2 rest1
  return ((r1, r2), rest2)

(>>>) :: Parser t a -> (a -> b) -> Parser t b
(p >>> f) input = (\(r, rest) -> (f r, rest)) <$> p input

(|||) :: Parser t a -> Parser t a -> Parser t a
(p1 ||| p2) input = case p1 input of { Nothing -> p2 input; r -> r }

many' :: Parser t a -> Parser t [a]
many' p input = case p input of
  Nothing -> Just ([], input)
  Just (r, next) -> case many' p next of
    Just (rs, rest) -> Just (r : rs, rest)
    Nothing -> Just ([], input)

atleastone :: Parser t a -> Parser t [a]
atleastone p = (p +++ many' p) >>> uncurry (:)

maybeP :: (t -> Maybe a) -> Parser t a
maybeP _ [] = Nothing
maybeP p (h : t) = (\r -> (r, t)) <$> p h

finished :: Parser t ()
finished [] = Just ((), [])
finished _ = Nothing

some' :: (t -> Bool) -> Parser t t
some' p = maybeP (\x -> if p x then Just x else Nothing)

exact :: Eq t => t -> Parser t t
exact tok = some' (== tok)

data Infix = Infix { tok :: String, prec :: Int, leftAssoc :: Bool }

layerops :: [Infix] -> [(Bool, [String])]
layerops = fst . foldl (flip unflatten) ([], 0) . sortBy' (\a b -> prec a < prec b)
  where unflatten (Infix t p la) ([], _) = ([(la, [t])], p)
        unflatten (Infix t p la) ((a, l) : dealt, p') =
          if p' == p then (if la == a then ((a, t : l) : dealt, p') else bug "infix parser/printer" "mixed assocs")
          else ((la, [t]) : (a, l) : dealt, p)

opClean :: String -> String
opClean t = let chop (' ' : chs) = (\(n, r) -> (n + 1, r)) (chop chs)
                chop chs = (0 :: Int, chs)
                (_, chs1) = chop (reverse t)
                (_, chs2) = chop (reverse chs1)
            in chs2

optoks :: [Infix] -> [String]
optoks = map (opClean . tok)

parseGenInfix :: ((a -> a) -> String -> a -> a -> a) -> (a -> a) -> [String] -> Parser String a -> Parser String a
parseGenInfix update sof toks parse inp = do
  (e, rest) <- parse inp
  case rest of
    h : t | mem h toks -> parseGenInfix update (update sof h e) toks parse t
    _ -> Just (sof e, rest)

parseLeftInfix, parseRightInfix :: [String] -> ((String, a, a) -> a) -> Parser String a -> Parser String a
parseLeftInfix toks con = parseGenInfix (\f t a b -> con (t, f a, b)) id toks
parseRightInfix toks con = parseGenInfix (\f t a b -> f (con (t, a, b))) id toks

parseInfixes :: [Infix] -> ((String, a, a) -> a) -> Parser String a -> Parser String a
parseInfixes ops =
  let layeredops = map (\(a, ts) -> (a, map opClean ts)) (layerops ops)
      iparser (a, t) = (if a then parseLeftInfix else parseRightInfix) t
      iparsers = map iparser layeredops
  in \con subparser -> foldl (\sp p -> p con sp) subparser iparsers

isSymbol, isPunct, isAlphanum :: Char -> Bool
isSymbol c = c `elem` "<>=-*+/\\?@|!$%&~#^:"
isPunct c = c `elem` "()[]{}.,;"
isAlphanum c = isAlpha c || isDigit c || c == '\'' || c == '_'

------------------------------------------------------------------------
-- Term1

data Term = Var String | Fn String [Term] deriving Eq

data Formula = FTrue | FFalse | Atom Term | Not Formula | And Formula Formula | Or Formula Formula
             | Imp Formula Formula | Iff Formula Formula | Forall String Formula | Exists String Formula
  deriving Eq

destAtom :: Formula -> Maybe Term
destAtom (Atom a) = Just a
destAtom _ = Nothing

listMkDisj :: [Formula] -> Formula
listMkDisj l = case reverse l of { [] -> FFalse; h : t -> foldl (flip Or) h t }

flattenConj :: Formula -> [Formula]
flattenConj fm = flat [] [fm]
  where flat acc [] = acc
        flat acc (And p q : fms) = flat acc (q : p : fms)
        flat acc (FTrue : fms) = flat acc fms
        flat acc (f : fms) = flat (f : acc) fms

listMkForall :: [String] -> Formula -> Formula
listMkForall [] body = body
listMkForall (v : vs) body = Forall v (listMkForall vs body)

stripForall :: Formula -> ([String], Formula)
stripForall = dest []
  where dest vs (Forall v b) = dest (v : vs) b
        dest vs tm = (reverse vs, tm)

infixes :: [Infix]
infixes =
  [ Infix " / " 7 True, Infix " div " 7 True, Infix " mod " 7 True, Infix " * " 7 True
  , Infix " + " 6 True, Infix " - " 6 True, Infix " ^ " 6 True, Infix " @ " 5 False
  , Infix " :: " 5 False, Infix " = " 4 True, Infix " == " 4 True, Infix " <> " 4 True
  , Infix " <= " 4 True, Infix " < " 4 True, Infix " >= " 4 True, Infix " > " 4 True
  , Infix " o " 8 True, Infix " % " 9 True, Infix " -> " 2 False, Infix " : " 1 False
  , Infix ", " 0 False, Infix " ** " 7 True, Infix " ++ " 6 True, Infix " -- " 6 True ]

connectives :: [Infix]
connectives = [Infix " /\\ " (-1) False, Infix " \\/ " (-2) False, Infix " ==> " (-3) False, Infix " <=> " (-4) False]

reserved :: [String]
reserved = ["!", "?", "(", ")", ".", "~"]

varString :: String -> Bool
varString s = toLower (head s) `elem` "_vwxyz"

lexer :: [Char] -> [String]
lexer = (\((_, (toks, _)), _) -> toks) . fromJust . p
  where
    p = many' (some' isSpace)
        +++ (many' ((((atleastone (some' isAlphanum)
                       ||| (some' (\c -> isSymbol c && c /= '~') +++ many' (some' isSymbol)) >>> uncurry (:))
                      >>> id
                      ||| some' (\c -> c == '~' || isPunct c) >>> (: []))
                     +++ many' (some' isSpace)) >>> fst))
        +++ finished

termParser :: [Infix] -> Parser String Term
termParser ops0 = tmParser
  where
    ops = ops0 ++ connectives
    iparser = parseInfixes ops
    itoks = optoks ops
    avoid = itoks ++ reserved
    fname t = not (mem t avoid) && not (varString t)
    uninfix t = mem t itoks
    uninfixParser = (exact "(" +++ some' uninfix +++ exact ")") >>> (fst . snd)
    fnameParser = some' fname ||| uninfixParser
    vnameParser = some' (\t -> not (mem t reserved) && varString t)
    bind s v t = Fn s [Var v, t]
    basic inp =
      ((exact "(" +++ tmParser +++ exact ")") >>> (\(_, (t, _)) -> t)
       ||| (exact "!" +++ atleastone vnameParser +++ exact "." +++ tmParser)
           >>> (\(_, (vs, (_, body))) -> foldr (bind "!") body vs)
       ||| (exact "?" +++ atleastone vnameParser +++ exact "." +++ tmParser)
           >>> (\(_, (vs, (_, body))) -> foldr (bind "?") body vs)
       ||| fnameParser >>> (\f -> Fn f [])
       ||| vnameParser >>> Var) inp
    molecule inp =
      ((many' (exact "~") +++ ((fnameParser +++ many' basic) >>> uncurry Fn ||| basic))
       >>> (\(l, t) -> nTimes (length l) (\x -> Fn "~" [x]) t)) inp
    tmParser inp = iparser (\(f, a, b) -> Fn f [a, b]) molecule inp

promote :: Term -> Formula
promote t = case t of
  Fn "T" [] -> FTrue
  Fn "F" [] -> FFalse
  Fn "~" [a] -> Not (promote a)
  Fn "/\\" [a, b] -> And (promote a) (promote b)
  Fn "\\/" [a, b] -> Or (promote a) (promote b)
  Fn "==>" [a, b] -> Imp (promote a) (promote b)
  Fn "<=>" [a, b] -> Iff (promote a) (promote b)
  Fn "!" [Var v, b] -> Forall v (promote b)
  Fn "?" [Var v, b] -> Exists v (promote b)
  _ -> Atom t

-- parse_formula of a quotation of one QUOTE (the problems have no
-- antiquotations), using !infixes
parseFormula :: String -> Formula
parseFormula s = case ((termParser infixes >>> promote) +++ finished) (lexer s) of
  Just ((f, ()), _) -> f
  Nothing -> error "Noparse"

newVars :: Int -> IO [Term]
newVars n = map (Var . ("_" ++) . smlInt) <$> newInts n

formulaSize :: Formula -> Int
formulaSize f0 = sz 0 [f0]
  where
    szt n [] = n
    szt n (Var _ : tms) = szt (n + 1) tms
    szt n (Fn _ args : tms) = szt (n + 1) (args ++ tms)
    sz n [] = n
    sz n (FTrue : fms) = sz (n + 1) fms
    sz n (FFalse : fms) = sz (n + 1) fms
    sz n (Atom t : fms) = sz (szt (n + 1) [t]) fms
    sz n (Not p : fms) = sz (n + 1) (p : fms)
    sz n (And p q : fms) = sz (n + 1) (p : q : fms)
    sz n (Or p q : fms) = sz (n + 1) (p : q : fms)
    sz n (Imp p q : fms) = sz (n + 1) (p : q : fms)
    sz n (Iff p q : fms) = sz (n + 1) (p : q : fms)
    sz n (Forall _ p : fms) = sz (n + 1) (p : fms)
    sz n (Exists _ p : fms) = sz (n + 1) (p : fms)

mkLiteral :: Bool -> Formula -> Formula
mkLiteral True a = a
mkLiteral False a = Not a

destLiteral :: Formula -> Maybe (Bool, Formula)
destLiteral a@(Atom _) = Just (True, a)
destLiteral (Not a@(Atom _)) = Just (False, a)
destLiteral _ = Nothing

isLiteral :: Formula -> Bool
isLiteral = isJust . destLiteral

literalAtom :: Formula -> Formula
literalAtom = snd . fromJust . destLiteral

negative, positive :: Formula -> Bool
negative (Not _) = True
negative _ = False
positive = not . negative

negateF :: Formula -> Formula
negateF (Not p) = p
negateF p = Not p

functions :: Formula -> [(String, Int)]
functions f0 = func [] [f0]
  where
    fnc fs [] = fs
    fnc fs (Var _ : tms) = fnc fs tms
    fnc fs (Fn n a : tms) = fnc (insertL (n, length a) fs) (a ++ tms)
    func fs [] = fs
    func fs (FTrue : fms) = func fs fms
    func fs (FFalse : fms) = func fs fms
    func fs (Atom (Var _) : fms) = func fs fms
    func fs (Atom (Fn _ tms) : fms) = func (fnc fs tms) fms
    func fs (Not p : fms) = func fs (p : fms)
    func fs (And p q : fms) = func fs (p : q : fms)
    func fs (Or p q : fms) = func fs (p : q : fms)
    func fs (Imp p q : fms) = func fs (p : q : fms)
    func fs (Iff p q : fms) = func fs (p : q : fms)
    func fs (Forall _ p : fms) = func fs (p : fms)
    func fs (Exists _ p : fms) = func fs (p : fms)

functionNames :: Formula -> [String]
functionNames = map fst . functions

relations :: Formula -> [(String, Int)]
relations f0 = rel [] [f0]
  where
    rel rs [] = rs
    rel rs (FTrue : fms) = rel rs fms
    rel rs (FFalse : fms) = rel rs fms
    rel rs (Atom (Var _) : fms) = rel rs fms
    rel rs (Atom (Fn n a) : fms) = rel (insertL (n, length a) rs) fms
    rel rs (Not p : fms) = rel rs (p : fms)
    rel rs (And p q : fms) = rel rs (p : q : fms)
    rel rs (Or p q : fms) = rel rs (p : q : fms)
    rel rs (Imp p q : fms) = rel rs (p : q : fms)
    rel rs (Iff p q : fms) = rel rs (p : q : fms)
    rel rs (Forall _ p : fms) = rel rs (p : fms)
    rel rs (Exists _ p : fms) = rel rs (p : fms)

eqRel :: (String, Int)
eqRel = ("=", 2)

mkEq :: Term -> Term -> Formula
mkEq a b = Atom (Fn "=" [a, b])

eqOccurs :: Formula -> Bool
eqOccurs = mem eqRel . relations

relationsNoEq :: Formula -> [(String, Int)]
relationsNoEq = filter (/= eqRel) . relations

fvt :: [String] -> [String] -> [Term] -> [String]
fvt av = f
  where seen = if null av then mem else (\v vs -> mem v av || mem v vs)
        f vs [] = vs
        f vs (Var v : tms) = f (if seen v vs then vs else v : vs) tms
        f vs (Fn _ args : tms) = f vs (args ++ tms)

fv :: [String] -> [([String], Formula)] -> [String]
fv vs [] = vs
fv vs ((_, FTrue) : fms) = fv vs fms
fv vs ((_, FFalse) : fms) = fv vs fms
fv vs ((av, Atom t) : fms) = fv (fvt av vs [t]) fms
fv vs ((av, Not p) : fms) = fv vs ((av, p) : fms)
fv vs ((av, And p q) : fms) = fv vs ((av, p) : (av, q) : fms)
fv vs ((av, Or p q) : fms) = fv vs ((av, p) : (av, q) : fms)
fv vs ((av, Imp p q) : fms) = fv vs ((av, p) : (av, q) : fms)
fv vs ((av, Iff p q) : fms) = fv vs ((av, p) : (av, q) : fms)
fv vs ((av, Forall x p) : fms) = fv vs ((insertL x av, p) : fms)
fv vs ((av, Exists x p) : fms) = fv vs ((insertL x av, p) : fms)

fvT :: Term -> [String]
fvT tm = reverse (fvt [] [] [tm])

fvF :: Formula -> [String]
fvF fm = reverse (fv [] [([], fm)])

fvL :: [Formula] -> [String]
fvL fms = reverse (fv [] (map (\f -> ([], f)) fms))

specialize :: Formula -> Formula
specialize = snd . stripForall

generalize :: Formula -> Formula
generalize fm = listMkForall (fvF fm) fm

subterm :: [Int] -> Term -> Term
subterm [] tm = tm
subterm (_ : _) (Var _) = error "subterm: Var"
subterm (h : t) (Fn _ args) = subterm t (args !! h)

rewrite :: ([Int], Term) -> Term -> Term
rewrite ([], res) _ = res
rewrite _ (Var _) = error "rewrite: Var"
rewrite (h : t, res) (Fn f args) = Fn f (update (rewrite (t, res)) h args)
  where update _ _ [] = error "rewrite: bad path"
        update g n (x : xs) = if n == 0 then g x : xs else x : update g (n - 1) xs

literalSubterm :: [Int] -> Formula -> Term
literalSubterm p = subterm p . fromJust . destAtom . literalAtom

literalRewrite :: ([Int], Term) -> Formula -> Formula
literalRewrite r lit = let (b, a) = fromJust (destLiteral lit)
                       in mkLiteral b (Atom (rewrite r (fromJust (destAtom a))))

------------------------------------------------------------------------
-- Subst1

newtype Subst = Subst (Dict String Term)

emptySubst :: Subst
emptySubst = Subst (mkDict compare)

(|->>) :: (String, Term) -> Subst -> Subst
(a, b) |->> Subst d = Subst (dInsert d a b)
infixr 5 |->>

nullSubst :: Subst -> Bool
nullSubst (Subst s) = numItems s == 0

findRedex :: String -> Subst -> Maybe Term
findRedex r (Subst d) = dPeek d r

purge :: String -> Subst -> Subst
purge v (Subst d) = Subst (maybe d fst (dRemove d v))

-- Nothing: the SML's Unchanged
tmSubst :: Subst -> Term -> Maybe Term
tmSubst r = f
  where f (Var v) = findRedex v r
        f (Fn n a) = Fn n <$> listUnchanged f a

listUnchanged :: (a -> Maybe a) -> [a] -> Maybe [a]
listUnchanged f xs = h (foldl g (False, []) xs)
  where g (b, l) x = case f x of { Just y -> (True, y : l); Nothing -> (b, x : l) }
        h (True, l) = Just (reverse l)
        h (False, _) = Nothing

pairUnchanged :: (a -> Maybe a) -> (a, a) -> Maybe (a, a)
pairUnchanged f (x, y) =
  let (c1, x') = case f x of { Just v -> (True, v); Nothing -> (False, x) }
      (c2, y') = case f y of { Just v -> (True, v); Nothing -> (c1, y) }
  in if c2 then Just (x', y') else Nothing

fmSubst :: Subst -> Formula -> Formula
fmSubst r = if nullSubst r then id else \fm -> maybe fm id (f fm)
  where
    f FFalse = Nothing
    f FTrue = Nothing
    f (Atom tm) = Atom <$> tmSubst r tm
    f (Not p) = Not <$> f p
    f (And p q) = uncurry And <$> pairUnchanged f (p, q)
    f (Or p q) = uncurry Or <$> pairUnchanged f (p, q)
    f (Imp p q) = uncurry Imp <$> pairUnchanged f (p, q)
    f (Iff p q) = uncurry Iff <$> pairUnchanged f (p, q)
    f (Forall v p) = Just (fmSubstq r Forall v p)
    f (Exists v p) = Just (fmSubstq r Exists v p)

fmSubstq :: Subst -> (String -> Formula -> Formula) -> String -> Formula -> Formula
fmSubstq r q v p =
  let v' = variant v (fvF (fmSubst (purge v r) p))
  in q v' (fmSubst ((v, Var v') |->> r) p)

termSubst :: Subst -> Term -> Term
termSubst env tm = if nullSubst env then tm else maybe tm id (tmSubst env tm)

formulaSubst :: Subst -> Formula -> Formula
formulaSubst = fmSubst

norm :: Subst -> Subst
norm sub@(Subst dict) =
  let check a b (c, d) = if Var a == b then (True, fst (fromJust (dRemove d a))) else (c, d)
      (removed, dict') = dFoldl check (False, dict) dict
  in if removed then Subst dict' else sub

toMaplets :: Subst -> [(String, Term)]
toMaplets (Subst s) = dListItems s

fromMaplets :: [(String, Term)] -> Subst
fromMaplets ms = foldl (flip (|->>)) emptySubst (reverse ms)

refine :: Subst -> Subst -> Subst
refine (Subst sub1) sub2 = foldl f sub2 (dListItems sub1)
  where f s (a, b) = let b' = termSubst sub2 b in if Var a == b' then s else (a, b') |->> s

isRenaming :: Subst -> Bool
isRenaming (Subst sub) = isJust (dFoldl rs (Just []) sub)
  where rs _ (Var w) (Just l) = if mem w l then Nothing else Just (w : l)
        rs _ _ _ = Nothing

substFoldr :: ((String, Term) -> b -> b) -> b -> Subst -> b
substFoldr f b (Subst sub) = dFoldr (\s t a -> f (s, t) a) b sub

------------------------------------------------------------------------
-- Kernel1 and Thm1

data Inference = Axiom | Refl | Assume | Inst | Factor | Resolve | Equality deriving Eq

data Thm = Thm [Formula] (Inference, [Thm]) deriving Eq

clause :: Thm -> [Formula]
clause (Thm cl _) = cl

axiomT :: [Formula] -> Thm
axiomT cl = if all isLiteral cl then Thm cl (Axiom, []) else error "AXIOM: argument not a list of literals"

reflT :: Term -> Thm
reflT tm = Thm [mkEq tm tm] (Refl, [])

assumeT :: Formula -> Thm
assumeT fm = if isLiteral fm then Thm [fm, negateF fm] (Assume, []) else error "ASSUME: argument not a literal"

instT :: Subst -> Thm -> Thm
instT env th@(Thm cl pr) =
  let cl' = map (formulaSubst env) cl
  in if cl' == cl then th
     else case pr of
       (Inst, [th']) -> if cl' == clause th' then th' else Thm cl' (Inst, [th'])
       _ -> Thm cl' (Inst, [th])

factorT :: Thm -> Thm
factorT th = let cl = reverse (setify (clause th)) in if cl == clause th then th else Thm cl (Factor, [th])

resolveT :: Formula -> Thm -> Thm -> Thm
resolveT fm th1 th2 =
  let cl1 = clause th1
      cl1' = filter (/= fm) cl1
      cl2 = clause th2
      cl2' = filter (/= negateF fm) cl2
  in if cl1 /= cl1' || cl2 /= cl2' then Thm (cl1' ++ cl2') (Resolve, [th1, th2])
     else error "RESOLVE: resolvant does not feature in either clause"

equalityT :: Formula -> [Int] -> Term -> Bool -> Thm -> Thm
equalityT fm p res lr th =
  let red = literalSubterm p fm
      eqLit = Not (if lr then mkEq red res else mkEq res red)
      l = clause th
      otherLits = case lookup True (zip (map (== fm) l) [0 ..]) of
        Nothing -> error "EQUALITY: literal does not occur in clause"
        Just n -> updateNth (literalRewrite (p, res)) n l
  in Thm (eqLit : otherLits) (Equality, [th])

isContradiction :: Thm -> Bool
isContradiction = null . clause

destUnit :: Thm -> Maybe Formula
destUnit th = case clause th of { [lit] -> Just lit; _ -> Nothing }

contrT :: Formula -> Thm -> Thm
contrT lit th = resolveT (negateF lit) (assumeT lit) th

freshVarsL :: [Thm] -> IO [Thm]
freshVarsL ths = do
  let fvs = fvL (concatMap clause ths)
  vvs <- newVars (length fvs)
  let sub = fromMaplets (zipwith (,) fvs vvs)
  return (map (instT sub) ths)

freshVars :: Thm -> IO Thm
freshVars th = freshVarsL [th] >>= \l -> case l of { [x] -> return x; _ -> error "unwrap" }

unitSquash :: Thm -> Maybe Thm
unitSquash th = (\env -> factorT (instT env th)) <$> squash emptySubst (clause th)
  where squash env (x : xs@(y : _)) = unifyLiterals env x y >>= \env' -> squash env' xs
        squash env _ = Just env

reflexivity, symmetry, transitivity :: Thm
reflexivity = reflT (Var "x")
symmetry = equalityT (mkEq (Var "x") (Var "x")) [0] (Var "y") True reflexivity
transitivity = equalityT (mkEq (Var "y") (Var "z")) [0] (Var "x") False (assumeT (Not (mkEq (Var "y") (Var "z"))))

funCongruence :: (String, Int) -> Thm
funCongruence (function, arity) =
  let xs = [Var ("x" ++ smlInt i) | i <- [0 .. arity - 1]]
      ys = [Var ("y" ++ smlInt i) | i <- [0 .. arity - 1]]
      f th i = equalityT (last (clause th)) [1, i] (ys !! i) True th
      refl = instT (("x", Fn function xs) |->> emptySubst) reflexivity
  in foldl f refl (reverse (interval 0 arity))

relCongruence :: (String, Int) -> Thm
relCongruence (relation, arity) =
  let xs = [Var ("x" ++ smlInt i) | i <- [0 .. arity - 1]]
      ys = [Var ("y" ++ smlInt i) | i <- [0 .. arity - 1]]
      f th i = equalityT (last (clause th)) [i] (ys !! i) True th
      refl = assumeT (Not (Atom (Fn relation xs)))
  in foldl f refl (reverse (interval 0 arity))

------------------------------------------------------------------------
-- Match1 (Nothing: the SML's ERR)

matchl :: Subst -> [(Term, Term)] -> Maybe Subst
matchl env [] = Just env
matchl env ((Var x, tm) : rest) = rawMatch >>= \e -> matchl e rest
  where rawMatch = case findRedex x env of
          Nothing -> Just ((x, tm) |->> env)
          Just tm' -> if tm == tm' then Just env else Nothing
matchl env ((Fn f args, Fn f' args') : rest) =
  if f == f' && length args == length args' then matchl env (zip args args' ++ rest) else Nothing
matchl _ _ = Nothing

convLits :: (Formula, Formula) -> Maybe (Maybe (Term, Term))
convLits (Atom t, Atom t') = Just (Just (t, t'))
convLits (Not p, Not q) = convLits (p, q)
convLits (FTrue, FTrue) = Just Nothing
convLits (FFalse, FFalse) = Just Nothing
convLits _ = Nothing

matchlLiterals :: Subst -> [(Formula, Formula)] -> Maybe Subst
matchlLiterals sub ps = mapM convLits ps >>= matchl sub . catMaybes

matchLiterals :: Formula -> Formula -> Maybe Subst
matchLiterals lit lit' = norm <$> matchlLiterals emptySubst [(lit, lit')]

unifyl :: Subst -> [(Term, Term)] -> Maybe Subst
unifyl = solve
  where
    solve env [] = Just env
    solve env ((tm1, tm2) : rest) = solve' env (termSubst env tm1) (termSubst env tm2) rest
    solve' env (Var x) tm rest
      | Var x == tm = solve env rest
      | mem x (fvT tm) = Nothing
      | otherwise = case findRedex x env of
          Nothing -> solve (refine env ((x, tm) |->> emptySubst)) rest
          Just tm' -> solve' env tm' tm rest
    solve' env tm tm'@(Var _) rest = solve' env tm' tm rest
    solve' env (Fn f args) (Fn f' args') rest =
      if f == f' && length args == length args' then solve env (zip args args' ++ rest) else Nothing

unifyLiterals :: Subst -> Formula -> Formula -> Maybe Subst
unifyLiterals env lit lit' = mapM convLits [(lit, lit')] >>= unifyl env . catMaybes

------------------------------------------------------------------------
-- TermNet1

data Pattern = VAR | FN String Int deriving Eq

data TermMap a = MAP Int [Tree Pattern (Int, a)]

tmEmpty :: TermMap a
tmEmpty = MAP 0 []

tmSize :: TermMap a -> Int
tmSize (MAP i _) = i

restoreFifoOrder :: [(Int, a)] -> [a]
restoreFifoOrder l = map snd (sortBy' (\(m, _) (n, _) -> m <= n) l)

tmToList :: TermMap a -> [a]
tmToList (MAP _ n) = restoreFifoOrder (concat (map (treeFoldr (const concat) (: [])) n))

tmInsert :: (Term, a) -> TermMap a -> TermMap a
tmInsert (tm, a) (MAP i n) = MAP (i + 1) (add (i, a) [tm] n)
  where
    findPat x (BRANCH p _) = p == x
    findPat _ (LEAF _) = bug "find_pat" "misplaced LEAF"
    partitionFind f l = pf [] l
      where pf _ [] = (Nothing, l)
            pf dealt (x : xs) = if f x then (Just x, reverse dealt ++ xs) else pf (x : dealt) xs
    add b [] l = LEAF b : l
    add b (t : rest) l =
      let (pat, rest') = case t of
            Var _ -> (VAR, rest)
            Fn f args -> (FN f (length args), args ++ rest)
          (this, others) = partitionFind (findPat pat) l
          next = case this of
            Nothing -> []
            Just (BRANCH _ l') -> l'
            Just (LEAF _) -> bug "add" "misplaced LEAF"
      in BRANCH pat (add b rest' next) : others

tmMatch :: TermMap a -> Term -> [a]
tmMatch (MAP _ n) tm = restoreFifoOrder (concat (map (treePartialFoldl mat final [tm]) n))
  where
    mat VAR (_ : rest) = Just rest
    mat (FN f k) (Fn g args : rest) = if f == g && k == length args then Just (args ++ rest) else Nothing
    mat (FN _ _) (Var _ : _) = Nothing
    mat _ [] = bug "match" "ran out of subterms"
    final a [] = Just a
    final _ (_ : _) = bug "match" "too many subterms"

more :: Pattern -> Int
more VAR = 0
more (FN _ k) = k

tmMatched :: TermMap a -> Term -> [a]
tmMatched (MAP _ n) tm = restoreFifoOrder (concat (map (treePartialFoldl mat final (0, [tm])) n))
  where
    mat pat (0, Var _ : rest) = Just (more pat, rest)
    mat VAR (0, Fn _ _ : _) = Nothing
    mat (FN f k) (0, Fn g args : rest) = if f == g && k == length args then Just (0, args ++ rest) else Nothing
    mat _ (0, []) = bug "matched" "ran out of subterms"
    mat pat (k, rest) = Just (more pat + k - 1, rest)
    final a (0, []) = Just a
    final _ (0, _ : _) = bug "matched" "too many subterms"
    final _ _ = bug "matched" "still skipping"

tmUnify :: TermMap a -> Term -> [a]
tmUnify (MAP _ n) tm = restoreFifoOrder (concat (map (treePartialFoldl mat final (0, [tm])) n))
  where
    mat pat (0, Var _ : rest) = Just (more pat, rest)
    mat VAR (0, Fn _ _ : rest) = Just (0, rest)
    mat (FN f k) (0, Fn g args : rest) = if f == g && k == length args then Just (0, args ++ rest) else Nothing
    mat _ (0, []) = bug "unify" "ran out of subterms"
    mat pat (k, rest) = Just (more pat + k - 1, rest)
    final a (0, []) = Just a
    final _ (0, _ : _) = bug "unify" "too many subterms"
    final _ _ = bug "unify" "still skipping"

------------------------------------------------------------------------
-- LiteralNet1

type LiteralMap a = ((TermMap a, TermMap a), ((Int, [a]), (Int, [a])))

lnEmpty :: LiteralMap a
lnEmpty = ((tmEmpty, tmEmpty), ((0, []), (0, [])))

lnInsert :: (Formula, a) -> LiteralMap a -> LiteralMap a
lnInsert (Atom a, b) ((p, n), tf) = ((tmInsert (a, b) p, n), tf)
lnInsert (Not (Atom a), b) ((p, n), tf) = ((p, tmInsert (a, b) n), tf)
lnInsert (FTrue, b) (pn, ((n, l), f)) = (pn, ((n + 1, b : l), f))
lnInsert (FFalse, b) (pn, (t, (n, l))) = (pn, (t, (n + 1, b : l)))
lnInsert _ _ = bug "insert" "not a lit"

lnFromMaplets :: [(Formula, a)] -> LiteralMap a
lnFromMaplets = foldl (flip lnInsert) lnEmpty

lnSize :: LiteralMap a -> Int
lnSize ((pos, neg), ((t, _), (f, _))) = t + f + tmSize pos + tmSize neg

lnFalsity :: LiteralMap a -> Int
lnFalsity (_, (_, (n, _))) = n

lnMatch, lnMatched, lnUnify :: LiteralMap a -> Formula -> [a]
lnMatch ((pos, _), _) (Atom a) = tmMatch pos a
lnMatch ((_, neg), _) (Not (Atom a)) = tmMatch neg a
lnMatch (_, ((_, t), _)) FTrue = reverse t
lnMatch (_, (_, (_, f))) FFalse = reverse f
lnMatch _ _ = bug "match" "not a literal"
lnMatched ((pos, _), _) (Atom a) = tmMatched pos a
lnMatched ((_, neg), _) (Not (Atom a)) = tmMatched neg a
lnMatched (_, ((_, t), _)) FTrue = reverse t
lnMatched (_, (_, (_, f))) FFalse = reverse f
lnMatched _ _ = bug "matched" "not a literal"
lnUnify ((pos, _), _) (Atom a) = tmUnify pos a
lnUnify ((_, neg), _) (Not (Atom a)) = tmUnify neg a
lnUnify (_, ((_, t), _)) FTrue = reverse t
lnUnify (_, (_, (_, f))) FFalse = reverse f
lnUnify _ _ = bug "unify" "not a literal"

------------------------------------------------------------------------
-- Subsume1

frozenPrefix :: String
frozenPrefix = "FROZEN__"

destFrozen :: Term -> Maybe String
destFrozen (Fn s []) = if frozenPrefix `isPrefixOf` s then Just (drop (length frozenPrefix) s) else Nothing
destFrozen _ = Nothing

freezeVars :: [Formula] -> [Formula]
freezeVars fms =
  let vars = fvF (listMkDisj fms)
      sub = foldl (\s v -> (v, Fn (frozenPrefix ++ v) []) |->> s) emptySubst vars
  in map (formulaSubst sub) fms

defrostVars :: Subst -> Subst
defrostVars = fromMaplets . map f . toMaplets
  where f (v, a) = (v, case destFrozen a of { Just x -> Var x; Nothing -> a })

sortLiteralsBySize :: [Formula] -> [Formula]
sortLiteralsBySize =
  map snd . sortBy' (\(m, _) (n, _) -> m <= n) . map (\lit -> (formulaSize (literalAtom lit), lit))

data Sinfo a = Sinfo { ssub :: Subst, shd :: Formula, stl :: [Formula], sfin :: a }

addLits :: Sinfo a -> LiteralMap (Sinfo a) -> LiteralMap (Sinfo a)
addLits i net = lnInsert (shd i, i) net

subsumes :: Bool -> LiteralMap (Sinfo a) -> [Formula] -> [(Subst, a)]
subsumes strict net0 lits0 = narrow (net0, [])
  where
    lits = freezeVars lits0
    accept s = let d = defrostVars s in if strict then (if isRenaming d then Nothing else Just d) else Just d
    litNet = lnFromMaplets (map (\l -> (l, ())) lits)
    impossible = any (null . lnMatched litNet)
    extend sub ls fin net =
      if impossible ls then net
      else case sortLiteralsBySize ls of
        [] -> bug "extend" "null"
        h : t -> addLits (Sinfo sub h t fin) net
    examine lit s@(net, res) (Sinfo sub h t fin) =
      case matchlLiterals sub [(h, lit)] of
        Nothing -> s
        Just sub' ->
          if null t then case accept sub' of { Just sub'' -> (net, (sub'', fin) : res); Nothing -> s }
          else (extend sub' (map (formulaSubst sub') t) fin net, res)
    narrow1 net s lit = foldl (examine lit) s (lnMatch net lit)
    narrow (net, res) = if lnSize net == 0 then res else narrow (foldl (narrow1 net) (lnEmpty, res) lits)

type Subsume a = Either a (LiteralMap (Sinfo a))

subsumeEmpty :: Subsume a
subsumeEmpty = Right lnEmpty

subsumeAdd :: ([Formula], a) -> Subsume a -> Subsume a
subsumeAdd _ s@(Left _) = s
subsumeAdd (fms, fin) (Right net) = case sortLiteralsBySize fms of
  [] -> Left fin
  h : t -> Right (addLits (Sinfo emptySubst h t fin) net)

subsumed :: Subsume a -> [Formula] -> [(Subst, a)]
subsumed (Left fin) _ = [(emptySubst, fin)]
subsumed (Right _) [] = []
subsumed (Right net) lits = subsumes False net lits

------------------------------------------------------------------------
-- Canon1

simplify1 :: Formula -> Formula
simplify1 fm = case fm of
  Not FFalse -> FTrue
  Not FTrue -> FFalse
  Not (Not f) -> f
  And FFalse _ -> FFalse
  And _ FFalse -> FFalse
  And FTrue q -> q
  And p FTrue -> p
  Or FFalse q -> q
  Or p FFalse -> p
  Or FTrue _ -> FTrue
  Or _ FTrue -> FTrue
  Imp FFalse _ -> FTrue
  Imp FTrue q -> q
  Imp _ FTrue -> FTrue
  Imp (Not p) FFalse -> p
  Imp p FFalse -> Not p
  Iff FTrue q -> q
  Iff p FTrue -> p
  Iff FFalse (Not q) -> q
  Iff FFalse q -> Not q
  Iff (Not p) FFalse -> p
  Iff p FFalse -> Not p
  Forall x p -> if mem x (fvF p) then fm else p
  Exists x p -> if mem x (fvF p) then fm else p
  _ -> fm

simplify :: Formula -> Formula
simplify fm = case fm of
  Not p -> simplify1 (Not (simplify p))
  And p q -> simplify1 (And (simplify p) (simplify q))
  Or p q -> simplify1 (Or (simplify p) (simplify q))
  Imp p q -> simplify1 (Imp (simplify p) (simplify q))
  Iff p q -> simplify1 (Iff (simplify p) (simplify q))
  Forall x p -> simplify1 (Forall x (simplify p))
  Exists x p -> simplify1 (Exists x (simplify p))
  _ -> fm

nnf, nnf' :: Formula -> Formula
nnf fm = case fm of
  And p q -> And (nnf p) (nnf q)
  Or p q -> Or (nnf p) (nnf q)
  Imp p q -> Or (nnf' p) (nnf q)
  Iff p q -> Or (And (nnf p) (nnf q)) (And (nnf' p) (nnf' q))
  Forall x p -> Forall x (nnf p)
  Exists x p -> Exists x (nnf p)
  Not x -> nnf' x
  _ -> fm
nnf' fm = case fm of
  FTrue -> FFalse
  FFalse -> FTrue
  And p q -> Or (nnf' p) (nnf' q)
  Or p q -> And (nnf' p) (nnf' q)
  Imp p q -> And (nnf p) (nnf' q)
  Iff p q -> Or (And (nnf p) (nnf' q)) (And (nnf' p) (nnf q))
  Forall x p -> Exists x (nnf' p)
  Exists x p -> Forall x (nnf' p)
  Not x -> nnf x
  _ -> Not fm

pullquants :: Formula -> Formula
pullquants fm = case fm of
  And (Forall x p) (Forall y q) -> pullquant2 Forall And x y p q
  Or (Exists x p) (Exists y q) -> pullquant2 Exists Or x y p q
  And (Forall x p) q -> pullquantL Forall And x p q
  And p (Forall x q) -> pullquantR Forall And x p q
  Or (Forall x p) q -> pullquantL Forall Or x p q
  Or p (Forall x q) -> pullquantR Forall Or x p q
  And (Exists x p) q -> pullquantL Exists And x p q
  And p (Exists x q) -> pullquantR Exists And x p q
  Or (Exists x p) q -> pullquantL Exists Or x p q
  Or p (Exists x q) -> pullquantR Exists Or x p q
  _ -> fm
  where
    ren x x' = formulaSubst ((x, Var x') |->> emptySubst)
    pullquantL q c x p r = let x' = variant x (fvF fm) in q x' (pullquants (c (ren x x' p) r))
    pullquantR q c x p r = let x' = variant x (fvF fm) in q x' (pullquants (c p (ren x x' r)))
    pullquant2 q c x _ p r = let x' = variant x (fvF fm) in q x' (pullquants (c (ren x x' p) (ren x x' r)))

prenex :: Formula -> Formula
prenex fm = case fm of
  Forall x p -> Forall x (prenex p)
  Exists x p -> Exists x (prenex p)
  And p q -> pullquants (And (prenex p) (prenex q))
  Or p q -> pullquants (Or (prenex p) (prenex q))
  _ -> fm

skolem :: [String] -> Formula -> Formula
skolem avoid fm = case fm of
  Exists y p ->
    let xs = subtractL (fvF p) [y]
        f = variant (if null xs then "c_" ++ y else "f_" ++ y) avoid
    in skolem avoid (formulaSubst ((y, Fn f (map Var xs)) |->> emptySubst) p)
  Forall x p -> Forall x (skolem avoid p)
  And p q -> skolem2 And p q
  Or p q -> skolem2 Or p q
  _ -> fm
  where skolem2 c p q = let p' = skolem avoid p
                            q' = skolem (unionL avoid (functionNames p')) q
                        in c p' q'

skolemize :: Formula -> Formula
skolemize fm = skolem (functionNames fm) fm

tautologous :: [Formula] -> Bool
tautologous cls = let (pos, neg) = partition positive cls in not (null (intersectL pos (map negateF neg)))

purecnf :: Formula -> [[Formula]]
purecnf (Or p q) = cartwith unionL (purecnf p) (purecnf q)
purecnf (And p q) = unionL (purecnf p) (purecnf q)
purecnf fm = [[fm]]

simpcnf :: Formula -> [[Formula]]
simpcnf FTrue = []
simpcnf FFalse = [[]]
simpcnf fm = filter (not . tautologous) (purecnf fm)

clausal :: Formula -> [[Formula]]
clausal = concat . map (simpcnf . specialize . prenex) . flattenConj . skolemize . nnf . simplify

axiomatize :: Formula -> [Thm]
axiomatize = map axiomT . clausal

eqAxiomatize :: Formula -> [Thm]
eqAxiomatize fm =
  [reflexivity, symmetry, transitivity] ++ map funCongruence (functions fm)
    ++ map relCongruence (relationsNoEq fm) ++ axiomatize fm

eqAxiomatize' :: Formula -> [Thm]
eqAxiomatize' fm = (if eqOccurs fm then eqAxiomatize else axiomatize) fm

------------------------------------------------------------------------
-- Units1

type Uns = LiteralMap Thm

type Units = Either Thm Uns

uadd :: Thm -> Uns -> Uns
uadd th uns = lnInsert (fromJust (destUnit th), th) uns

usubsumes :: Uns -> Formula -> Maybe Thm
usubsumes uns lit =
  case filter (\th -> isJust (matchLiterals (fromJust (destUnit th)) lit)) (reverse (lnMatch uns lit)) of
    th : _ -> Just th
    [] -> Nothing

uprove :: Uns -> [Formula] -> Maybe [Thm]
uprove uns = mapM pr
  where pr lit = (\th -> instT (fromJust (matchLiterals (fromJust (destUnit th)) lit)) th) <$> usubsumes uns lit

udemod :: Uns -> Thm -> Thm
udemod uns th0 = foldl demod th0 (clause th0)
  where demod th lit = case uprove uns [negateF lit] of
          Nothing -> th
          Just [dth] -> resolveT lit th dth
          Just _ -> bug "unit_demod" "corrupt"

unitsEmpty :: Units
unitsEmpty = Right lnEmpty

unitsProve :: Units -> [Formula] -> Maybe [Thm]
unitsProve (Left th) = Just . map (\l -> case l of { FFalse -> th; lit -> contrT lit th })
unitsProve (Right uns) = uprove uns

unitsInfo :: Units -> String
unitsInfo (Left _) = "*"
unitsInfo (Right uns) = smlInt (lnSize uns)

unitsAdd :: Thm -> Units -> Units
unitsAdd _ u@(Left _) = u
unitsAdd th u@(Right uns) =
  if any (isJust . usubsumes uns) (clause th) then u
  else let th' = udemod uns th
       in if isContradiction th' then Left th'
          else case unitSquash th' of { Nothing -> u; Just th'' -> Right (uadd th'' uns) }

------------------------------------------------------------------------
-- Stream: the solvers' streams, whose tails are effectful thunks

data Stream a = NIL | CONS a (IO (Stream a))

mapS :: (a -> IO b) -> Stream a -> IO (Stream b)
mapS _ NIL = return NIL
mapS f (CONS h t) = f h >>= \h' -> return (CONS h' (t >>= mapS f))

partialMapS :: (a -> IO (Maybe b)) -> Stream a -> IO (Stream b)
partialMapS _ NIL = return NIL
partialMapS f (CONS h t) = f h >>= \r -> case r of
  Nothing -> t >>= partialMapS f
  Just h' -> return (CONS h' (t >>= partialMapS f))

partialMapsS :: (a -> s -> (Maybe b, s)) -> s -> Stream a -> IO (Stream b)
partialMapsS _ _ NIL = return NIL
partialMapsS f s (CONS x xs) =
  let (yo, s') = f x s
      t = xs >>= partialMapsS f s'
  in case yo of { Nothing -> t; Just y -> return (CONS y t) }

flattenS :: Stream (Stream a) -> IO (Stream a)
flattenS NIL = return NIL
flattenS (CONS NIL ss) = ss >>= flattenS
flattenS (CONS (CONS x xs) ss) = return (CONS x (xs >>= \xs' -> flattenS (CONS xs' ss)))

-- Solver1.drop_after
dropAfter :: (a -> IO Bool) -> Stream a -> IO (Stream a)
dropAfter _ NIL = return NIL
dropAfter f (CONS x xs) = f x >>= \stop -> return (CONS x (if stop then return NIL else xs >>= dropAfter f))

------------------------------------------------------------------------
-- Meter1

data Limit = Limit { ltime :: Maybe Double, linfs :: Maybe Int }

unlimited, expired :: Limit
unlimited = Limit Nothing Nothing
expired = Limit (Just 0.0) (Just 0)

limitToString :: Limit -> String
limitToString (Limit time infs) =
  "{time = " ++ maybe "unlimited" (\r -> showFFloat (Just 3) r "" ++ "s") time
    ++ ", infs = " ++ maybe "unlimited" smlInt infs ++ "}"

data Reading = Reading { rtime :: Double, rinfs :: Int }

zeroReading :: Reading
zeroReading = Reading 0.0 0

addReadings :: Reading -> Reading -> Reading
addReadings (Reading t i) (Reading t' i') = Reading (t + t') (i + i')

data Meter = Meter { mread :: IO Reading, mlog :: Int -> IO (), mlim :: Limit }

cpuSeconds :: IO Double
cpuSeconds = (\ps -> fromIntegral ps / 1.0e12) <$> getCPUTime

newMeter :: Limit -> IO Meter
newMeter lim = do
  t0 <- cpuSeconds
  infs <- newIORef (0 :: Int)
  return Meter { mread = do { t <- cpuSeconds; i <- readIORef infs; return (Reading (t - t0) i) }
               , mlog = \n -> modifyIORef' infs (+ n), mlim = lim }

subMeter :: Meter -> Limit -> IO Meter
subMeter m lim = do
  Reading t0 i0 <- mread m
  return m { mread = (\(Reading t i) -> Reading (t - t0) (i - i0)) <$> mread m, mlim = lim }

checkMeter :: Meter -> IO Bool
checkMeter m = do
  Reading t i <- mread m
  let Limit time infs = mlim m
  return (maybe True (t <) time && maybe True (i <) infs)

recordInfs :: Meter -> Int -> IO ()
recordInfs = mlog

------------------------------------------------------------------------
-- Solver1

chatS :: Int -> String -> IO ()
chatS l m = trace' "Solver1" m l

type Solver = [Formula] -> IO (Stream (Maybe [Thm]))

contradictionSolver :: Thm -> Solver
contradictionSolver th
  | isContradiction th = \gs -> return (CONS (Just (contr gs)) (return NIL))
  | otherwise = error "contradiction_solver: thm not |- F"
  where contr [FFalse] = [th]
        contr gs = map (`contrT` th) gs

solvedFilter :: Solver -> Solver
solvedFilter solver goals = solver goals >>= dropAfter (return . final)
  where concl [] = FFalse
        concl [lit] = lit
        concl _ = bug "concl" "not a literal"
        solves goals' = isJust (matchlLiterals emptySubst (zip goals' goals))
        final Nothing = False
        final (Just ths) = solves (map (concl . clause) ths)

subsumedFilter :: Solver -> Solver
subsumedFilter s g = s g >>= partialMapsS advance subsumeEmpty
  where
    munge p n = "MUNGED__" ++ smlInt n ++ "__" ++ p
    mungeLit (n, Atom (Fn p a)) = Atom (Fn (munge p n) a)
    mungeLit (n, Not (Atom (Fn p a))) = Not (Atom (Fn (munge p n) a))
    mungeLit _ = bug "munge_lit" "bad literal"
    distinctivize fms = map mungeLit (zip [0 ..] fms)
    advance Nothing st = (Just Nothing, st)
    advance (Just ths) st =
      let fms = distinctivize (mapMaybe destUnit ths)
      in if not (null (subsumed st fms)) then (Nothing, st) else (Just (Just ths), subsumeAdd (fms, ()) st)

rawSolve :: Solver -> [Formula] -> IO (Stream [Thm])
rawSolve s g = subsumedFilter (solvedFilter s) g >>= partialMapS return

findS :: Solver -> [Formula] -> IO (Maybe [Thm])
findS s g = rawSolve s g >>= \st -> return (case st of { NIL -> Nothing; CONS x _ -> Just x })

refute :: Solver -> IO (Maybe Thm)
refute s = fmap (\l -> case l of { [x] -> x; _ -> error "unwrap" }) <$> findS s [FFalse]

data Form = Form { fslice :: IORef Meter, funits :: IORef Units, fthms :: [Thm], fhyps :: [Thm] }

data SolverNode = SolverNode { sname :: String, sinitial :: String, solverCon :: Form -> IO Solver }

mkSolverNode :: String -> (Form -> IO Solver) -> SolverNode
mkSolverNode name con = SolverNode name [head name] con

sliceRef :: IORef Limit
sliceRef = unsafePerformIO (newIORef (Limit (Just (1.0 / 3.0)) Nothing))
{-# NOINLINE sliceRef #-}

type CostFn = Reading -> Double

time1 :: CostFn
time1 = rtime

data Subnode = Subnode { nname :: String, nused :: Reading, ncost :: Reading -> Double
                       , nsolns :: Maybe (IO (Stream (Maybe [Thm]))) }

timeToString :: Double -> String
timeToString t = let dp = if t < 10.0 then 2 else if t < 1000.0 then 1 else 0 in showFFloat (Just dp) t ""

infsToString :: Int -> String
infsToString i
  | i < 10000 = smlInt i
  | i < 10000000 = smlInt (i `div` 1000) ++ "K"
  | otherwise = smlInt (i `div` 1000000) ++ "M"

subnodeInfo :: Subnode -> String
subnodeInfo (Subnode name (Reading time infs) _ solns) =
  [head name] ++ "(" ++ timeToString time ++ "," ++ infsToString infs ++ ")" ++ maybe "*" (const "") solns

statusInfo :: [Subnode] -> Units -> String
statusInfo subnodes units = "[" ++ sq subnodes ++ "]--u=" ++ unitsInfo units ++ "--"
  where sq [] = ""
        sq (h : t) = foldl (\s n -> s ++ "--" ++ subnodeInfo n) (subnodeInfo h) t

chooseSubnode :: [Subnode] -> Maybe Int
chooseSubnode nodes = case filter (isJust . nsolns . snd) (zip [0 ..] nodes) of
  [] -> Nothing
  l -> Just (snd (minBy (\(r, _) (s, _) -> r <= s) (map (\(n, sn) -> (ncost sn (nused sn), n)) l)))

schedule :: IO Bool -> IO Reading -> ([Subnode] -> IO String) -> [Subnode] -> IO (Stream (Maybe [Thm]))
schedule check readR stat = sched
  where
    sched nodes = do
      stat nodes >>= chatS 2
      ok <- check
      if not ok then chatS 1 "?\n" >> return (CONS Nothing (sched nodes))
      else case chooseSubnode nodes of
        Nothing -> chatS 1 "!\n" >> return NIL
        Just n -> do
          let Subnode name used cost solns = nodes !! n
          chatS 1 name
          sq <- fromJust solns
          r <- readR
          chatS 2 ("--t=" ++ timeToString (rtime r) ++ "\n")
          let used' = addReadings used r
              (res, solns') = case sq of { NIL -> (Nothing, Nothing); CONS a rest -> (a, Just rest) }
              nodes' = updateNth (const (Subnode name used' cost solns')) n nodes
          case res of
            Nothing -> return ()
            Just _ -> stat nodes' >>= chatS 2 >> chatS 1 "$\n"
          return (CONS res (sched nodes'))

combine :: [(CostFn, SolverNode)] -> SolverNode
combine csolvers = SolverNode n i (combineSolvers n csolvers)
  where sq _ [] = ""
        sq f (h : t) = foldl (\s x -> s ++ "," ++ f x) (f h) t
        n = "[" ++ sq (sname . snd) csolvers ++ "]"
        i = "[" ++ sq (sinitial . snd) csolvers ++ "]"

combineSolvers :: String -> [(CostFn, SolverNode)] -> Form -> IO Solver
combineSolvers n csolvers (Form slice units thms hyps) = do
  chatS 2 (n ++ "--initializing--#thms=" ++ smlInt (length thms) ++ "--#hyps=" ++ smlInt (length hyps) ++ ".\n")
  meter <- newMeter expired >>= newIORef
  cnodes <- mapM (\(cost, sn) -> solverCon sn (Form meter units thms hyps) >>= \s -> return (cost, (sinitial sn, s))) csolvers
  let check = do
        sl <- readIORef slice
        ok <- checkMeter sl
        if ok then readIORef sliceRef >>= subMeter sl >>= writeIORef meter >> return True else return False
      readR = readIORef meter >>= mread
      stat s = statusInfo s <$> readIORef units
  return (\goal -> schedule check readR stat
                     (map (\(cost, (name, solver)) -> Subnode name zeroReading cost (Just (solver goal))) cnodes))

setOfSupport :: (Thm -> Bool) -> SolverNode -> SolverNode
setOfSupport filt (SolverNode name initial con) = SolverNode name' initial sos
  where
    name' = "!" ++ name
    sos (Form slice units thms hyps) = do
      chatS 2 (name' ++ "--initializing--#thms=" ++ smlInt (length thms) ++ "--#hyps=" ++ smlInt (length hyps) ++ ".\n")
      let (hyps', thms') = partition filt (thms ++ hyps)
      con (Form slice units thms' hyps')

allNegative :: Thm -> Bool
allNegative = all negative . clause

initialize :: SolverNode -> Limit -> [Thm] -> [Thm] -> IO Solver
initialize node limit thms hyps =
  case filter isContradiction (thms ++ hyps) of
    th : _ -> return (contradictionSolver th)
    [] -> do
      meter <- newMeter expired >>= newIORef
      units <- newIORef unitsEmpty
      solver <- solverCon node (Form meter units thms hyps)
      return (\g -> do
        newMeter limit >>= writeIORef meter
        solver g >>= dropAfter (\_ -> not <$> (readIORef meter >>= checkMeter)))

------------------------------------------------------------------------
-- Meson1

chatM :: Int -> String -> IO ()
chatM l m = trace' "Meson1" m l

data Parameters = Parameters { ancestorPruning, ancestorCutting, stateSimplify, cacheCutting
                             , divideConquer, unitLemmaizing :: Bool }

mesonDefaults :: Parameters
mesonDefaults = Parameters True True True True True True

halves :: Int -> (Int, Int)
halves n = let n1 = n `div` 2 in (n1, n - n1)

splittable :: [a] -> Bool
splittable (_ : _ : _) = True
splittable _ = False

mkMvar :: Int -> String
mkMvar i = "_m" ++ smlInt i

destMvar :: String -> Maybe Int
destMvar v = if "_m" `isPrefixOf` v then Just (stringToInt (drop 2 v)) else Nothing

-- A choice point: the next result and the choices after it
newtype Choice = Choice (IO R)
type R = (Maybe [Thm], Choice)

noChoice :: IO R
noChoice = throwErr "no_choice" "always fails"

binaryChoice :: IO R -> IO R -> IO R
binaryChoice f g = (f >>= \(a, Choice c) -> return (a, Choice (binaryChoice c g))) `catchErr` g

firstChoice :: [IO R] -> IO R
firstChoice [] = noChoice
firstChoice [f] = f
firstChoice (f : fs) = binaryChoice f (firstChoice fs)

choiceStream :: IO R -> IO (Stream (Maybe [Thm]))
choiceStream f = (f >>= \(a, Choice c) -> return (CONS a (choiceStream c))) `catchErr` return NIL

swivel :: Int -> Int -> [a] -> [a]
swivel m n l = let (l1, l') = split l m
                   (l2, l3) = split l' n
               in l2 ++ l1 ++ l3

thmProves :: Thm -> Formula -> Bool
thmProves th FFalse = isContradiction th
thmProves th goal = case clause th of { [lit] -> lit == goal; [] -> True; _ -> False }

filterMeter :: IORef Meter -> Stream (Maybe [Thm]) -> IO (Stream (Maybe [Thm]))
filterMeter meter = partialMapS (\a -> if isJust a then return (Just a)
                                       else (\ok -> if not ok then Just a else Nothing) <$> (readIORef meter >>= checkMeter))

data Rule = Rule { rasms :: [Formula], rc :: Formula, rthm :: Thm, rasmn :: Int }

newtype Rules = Rules (LiteralMap Rule)

addContrapositives :: ([Formula] -> [Formula]) -> Bool -> Thm -> Rules -> IO Rules
addContrapositives chosen sos th0 (Rules ruls) = do
  th <- freshVars th0
  let lits = clause th
      lits' = map negateF lits
      base = map (\l -> (subtractL lits' [negateF l], l)) (chosen lits)
      contrs = if sos then (lits', FFalse) : base else base
      f (a, c) = (c, Rule a c th (length a))
  return (Rules (foldl (\t h -> lnInsert (f h) t) ruls contrs))

mesonRules :: [Thm] -> [Thm] -> IO Rules
mesonRules thms hyps = do
  r0 <- foldlIO (\r th -> addContrapositives id False th r) (Rules lnEmpty) thms
  foldlIO (\r th -> addContrapositives id True th r) r0 hyps
  where foldlIO f z xs = foldl (\acc x -> acc >>= \a -> f a x) (return z) xs

numInitialRules, numRules :: Rules -> Int
numInitialRules (Rules r) = lnFalsity r
numRules rs@(Rules r) = lnSize r - numInitialRules rs

data St = St { env :: Subst, depth :: Int, proof :: [Thm], offset :: Int }

type Cont = St -> IO R
type Expand = [Formula] -> Formula -> Cont -> St -> IO R

ancestorPrune, ancestorCut :: Bool -> Subst -> Formula -> [Formula] -> Bool
ancestorPrune False _ _ = const False
ancestorPrune True e g = let g' = formulaSubst e g in any ((== g') . formulaSubst e)
ancestorCut False _ _ = const False
ancestorCut True e g = let g' = negateF (formulaSubst e g) in any ((== g') . formulaSubst e)

cacheCont :: Cont -> St -> IO Cont
cacheCont c st0 = do
  memRef <- newIORef []
  let f v = case destMvar v of { Nothing -> True; Just n -> n < offset st0 }
      listify = substFoldr (\m@(v, _) acc -> if f v then m : acc else acc) []
      purify s = do
        let l = listify (env s)
            n = depth s
        m <- readIORef memRef
        if any (\(n', l') -> n <= n' && l == l') m then throwErr "cache_cut" "repetition"
        else do
          writeIORef memRef ((n, l) : m)
          return s { env = fromMaplets l }
  return (\s -> purify s >>= c)

cacheCut :: Bool -> Expand -> Expand
cacheCut False f = f
cacheCut True f = \a g c s -> cacheCont c s >>= \c' -> f a g c' s

grabUnit :: IORef Units -> St -> IO St
grabUnit units s = case proof s of
  th : _ -> modifyIORef units (unitsAdd th) >> return s
  [] -> bug "grab_unit" "no thms"

useUnit :: IORef Units -> Formula -> Cont -> St -> IO R
useUnit units g c s = do
  u <- readIORef units
  ths <- maybe (throwErr "use_unit" "NONE") return (unitsProve u [formulaSubst (env s) g])
  th <- case ths of { [x] -> return x; _ -> throwErr "unwrap" "not a singleton" }
  c s { proof = th : proof s }

unitCut :: Bool -> IORef Units -> Expand -> Expand
unitCut False _ f = f
unitCut True units f = \a g c s -> useUnit units g c s `catchErr` f a g (\st -> grabUnit units st >>= c) s

freshenRule :: Rule -> Int -> ((Thm, [Formula], Formula), Int)
freshenRule (Rule asms c th _) i =
  let fvs = fvL (c : asms)
      fvn = length fvs
      mvs = map (Var . mkMvar) (interval i fvn)
      sub = fromMaplets (zipwith (,) fvs mvs)
  in ((instT sub th, map (formulaSubst sub) asms, formulaSubst sub c), i + fvn)

reward :: Int -> St -> St
reward r s = s { depth = depth s + r }

spend :: Int -> (Cont -> St -> IO R) -> Cont -> St -> IO R
spend m f c s = do
  let low = depth s - m
  unless (0 <= low) (throwErr "meson" "impossible divide and conquer")
  f (\s' -> if depth s' <= low then c s' else throwErr "meson" "divide and conquer") s

nextState :: Bool -> Subst -> (Thm, [Formula], Formula) -> Formula -> IO (Thm, [Formula], Subst)
nextState simp e r@(th, asms, c) g =
  case (if simp then matchLiterals c g else Nothing) of
    Just sub | simp -> return (instT sub th, map (formulaSubst sub) asms, e)
    _ -> unify
  where unify = (\e' -> (th, asms, e')) <$> orThrow "unify" (unifyLiterals e c g)
        _ = r

modusPonens :: Thm -> [Formula] -> St -> St
modusPonens th gs state = state { proof = mp (instT (env state) th) (reverse gs) (proof state) }
  where mp t [] p = factorT t : p
        mp t (g : gs') (th1 : p) = mp (resolveT (formulaSubst (env state) g) (instT (env state) th1) t) gs' p
        mp _ (_ : _) [] = bug "modus_ponens" "fresh out of thms"

data System = System { parm :: Parameters, rules :: Rules, meter :: IORef Meter, saturated :: IORef Bool
                     , cut :: Expand -> Expand }

mesonExpand :: System -> Formula -> Cont -> St -> IO R
mesonExpand (System prm (Rules ruls) meterRef sat cutF) = cutF expand []
  where
    expand ancestors g cont state = do
      ok <- readIORef meterRef >>= checkMeter
      if not ok then return (Nothing, Choice (expand ancestors g cont state))
      else if ancestorPrune (ancestorPruning prm) (env state) g ancestors then throwErr "meson" "ancestor pruning"
      else if ancestorCut (ancestorCutting prm) (env state) g ancestors then do
        readIORef meterRef >>= \m -> recordInfs m 1
        cont state { proof = assumeT g : proof state }
      else do
        let reduction a = do
              e' <- orThrow "unify" (unifyLiterals (env state) g (negateF a))
              let state' = state { env = e', proof = assumeT g : proof state }
              readIORef meterRef >>= \m -> recordInfs m 1
              cont state'
            expansion = expandRule ancestors g cont state
        firstChoice (map reduction ancestors ++ map expansion (lnUnify ruls (formulaSubst (env state) g)))
    expandRule ancestors g cont (St e d p o) r = do
      let d' = d - rasmn r
      unless (0 <= d') (writeIORef sat False >> throwErr "meson" "too deep")
      let (r', o') = freshenRule r o
      (th, asms, e') <- nextState (stateSimplify prm) e r' g
      readIORef meterRef >>= \m -> recordInfs m 1
      expands (g : ancestors) asms (cont . modusPonens th asms) (St e' d' p o')
    expands ancestors g c s =
      if divideConquer prm && splittable g then do
        let (l1, l2) = halves (length g)
            (g1, g2) = split g l1
            f1 = expands ancestors g1
            f2 = expands ancestors g2
            (n1, n2) = halves (depth s)
            s' = s { depth = n1 }
        binaryChoice (f1 (f2 c . reward n2) s')
                     (f2 (spend (n1 + 1) f1 (c . (\st -> st { proof = swivel l1 l2 (proof st) })) . reward n2) s')
      else foldl (\c' g' -> cutF expand ancestors g' c') c (reverse g) s

mesonFinally :: [Formula] -> St -> IO R
mesonFinally g (St e _ p _) = do
  unless (length p == length g) (bug "meson" "bad final state")
  let g' = map (formulaSubst e) g
      proof' = map (instT e) (reverse p)
  unless (and (zipWith thmProves proof' g')) (bug "meson" "did not prove goal list")
  ths <- freshVarsL proof'
  return (Just ths, Choice noChoice)

rawMeson :: System -> [Formula] -> Int -> IO (Stream (Maybe [Thm]))
rawMeson system goals d =
  choiceStream (foldl (\c g -> mesonExpand system g c) (mesonFinally goals) (reverse goals) (St emptySubst d [] 0))

mkSystem :: Parameters -> IORef Units -> IORef Meter -> Rules -> IO System
mkSystem prm units m ruls = do
  sat <- newIORef False
  return (System prm ruls m sat (unitCut (unitLemmaizing prm) units . cacheCut (cacheCutting prm)))

meson' :: Parameters -> SolverNode
meson' prm = mkSolverNode "meson" con
  where
    con (Form slice units thms hyps) = do
      ruls <- mesonRules thms hyps
      chatM 2 ("meson--initializing--#thms=" ++ smlInt (length thms) ++ "--#hyps=" ++ smlInt (length hyps)
               ++ "--#rules=" ++ smlInt (numRules ruls) ++ "--#initial_rules=" ++ smlInt (numInitialRules ruls) ++ ".\n")
      system <- mkSystem prm units slice ruls
      let b = saturated system
          d n = readIORef b >>= \s -> if s then return NIL else writeIORef b True >> return (CONS n (d (n + 1)))
          f q dd = chatM 1 ("-" ++ smlInt dd) >> rawMeson system q dd
          unitCheck goals Nothing = (`unitsProve` goals) <$> readIORef units
          unitCheck _ s = return s
      return (\goals -> d 0 >>= mapS (f goals) >>= flattenS >>= mapS (unitCheck goals) >>= filterMeter slice)

------------------------------------------------------------------------
-- Metis1

data Settings = Settings { smeson, sdelta, sresolution :: Bool, mesonParm :: Parameters }

settingsRef :: IORef Settings
settingsRef = unsafePerformIO (newIORef (Settings True True True mesonDefaults))
{-# NOINLINE settingsRef #-}

limitRef :: IORef Limit
limitRef = unsafePerformIO (newIORef unlimited)
{-# NOINLINE limitRef #-}

-- metis' with meson only (resolution and delta are not translated)
metis' :: Settings -> SolverNode
metis' (Settings m d r mp)
  | r || d = error "resolution and delta are not translated"
  | otherwise = combine ((if m then ((time1, meson' mp) :) else id) [])

rawProve :: Formula -> IO (Maybe Thm)
rawProve (Imp a (Imp b FFalse)) = do
  let thms = axiomatize a
      hyps = axiomatize b
  solv <- metis' <$> readIORef settingsRef
  lim <- readIORef limitRef
  initialize solv lim thms hyps >>= refute
rawProve _ = throwErr "raw_prove" "formula not of type a ==> b ==> F"

prove :: Formula -> IO (Maybe Thm)
prove g = do
  let hyps = eqAxiomatize' (Not (generalize g))
  solv <- setOfSupport allNegative . metis' <$> readIORef settingsRef
  lim <- readIORef limitRef
  initialize solv lim [] hyps >>= refute

------------------------------------------------------------------------
-- Main

-- Milton.time
miltonTime :: (a -> IO b) -> a -> IO b
miltonTime f x = do
  c0 <- cpuSeconds
  r0 <- getMonotonicTime
  let p t = showFFloat (Just 3) t ""
      pt = do
        c <- cpuSeconds
        r <- getMonotonicTime
        prn ("User: " ++ p (c - c0) ++ "  System: " ++ p 0 ++ "  Real: " ++ p (r - r0) ++ "\n")
  y <- f x `onException` pt
  pt
  return y

advertize :: String -> IO ()
advertize s = prn ("==" ++ s ++ replicate (77 - length s) '=' ++ "\n\n")

separator :: IO ()
separator = prn (replicate 79 '-' ++ "\n\n")

showSettings :: Settings -> Limit -> String
showSettings (Settings m d r mp) lim =
  "resolution             = " ++ b2s r ++ "\n" ++
  "meson                  = " ++ b2s m ++ "\n" ++
  "delta                  = " ++ b2s d ++ "\n" ++
  "\n" ++
  "resolution_parm:\n" ++
  "  subsumption_checking = " ++ smlInt 1 ++ "\n" ++
  "  positive_refinement  = " ++ b2s True ++ "\n" ++
  "  theap_parm:\n" ++
  "    fifo_skew          = " ++ smlInt 3 ++ "\n" ++
  "    theap_cleaning     = " ++ smlInt 1000 ++ "\n" ++
  "\n" ++
  "meson_parm:\n" ++
  "  ancestor_pruning     = " ++ b2s (ancestorPruning mp) ++ "\n" ++
  "  ancestor_cutting     = " ++ b2s (ancestorCutting mp) ++ "\n" ++
  "  state_simplify       = " ++ b2s (stateSimplify mp) ++ "\n" ++
  "  cache_cutting        = " ++ b2s (cacheCutting mp) ++ "\n" ++
  "  divide_conquer       = " ++ b2s (divideConquer mp) ++ "\n" ++
  "  unit_lemmaizing      = " ++ b2s (unitLemmaizing mp) ++ "\n" ++
  "\n" ++
  "limit                  = " ++ limitToString lim ++ "\n\n"
  where b2s True = "on"
        b2s False = "off"

-- Main.main (Resolution1.defaults, printed by show, are constants above)
mainOnce :: ([(String, String)], [(String, String)], [(String, String)]) -> IO ()
mainOnce (nonequality, equality, tptp) = do
  cnfNormalization <- newIORef False
  let withCnf b = withFlag cnfNormalization (const b)
      coreProve fm = do
        cnf <- readIORef cnfNormalization
        r <- (if cnf then prove else rawProve) fm
        prn (if isJust r then "METIS: SUCCESSFULLY PROVED\nMETIS: " else "METIS: FAILED TO PROVE\nMETIS: ")
      process name goal = do
        prn ("METIS: Problem " ++ name ++ "\n")
        miltonTime coreProve goal
        prn "\n"
      processSet (n, s) = do
        advertize n
        let f (name, goal) = process name (parseFormula goal)
        case s of
          [] -> return ()
          p : ps -> f p >> forM_ ps (\x -> separator >> f x)
  -- options (): everything off; then pure turns meson on
  modifyIORef settingsRef (\s -> s { sresolution = False })
  modifyIORef settingsRef (\s -> s { smeson = False })
  modifyIORef settingsRef (\s -> s { sdelta = False })
  modifyIORef settingsRef (\s -> s { smeson = True })
  Settings m _ r _ <- readIORef settingsRef
  let extract p n = fromJust (lookup n p)
      mesonPrune = ["P29", "LDA007-3", "GRP010-4", "GEO002-4"]
      prune = filter (not . (case (m, r) of
        (False, False) -> const True
        (False, True) -> (`mem` ["COL060-3"])
        (True, False) -> (`mem` mesonPrune)
        (True, True) -> const False))
      src0 = ["P26", "P29", "P46", "GILMORE_1", "LOS", "STEAM_ROLLER"]
      src1 = ["P48", "P49", "AGATHA"]
      src2 = ["LCL009-1", "COL060-3", "COL058-2", "LCL107-1", "LDA007-3", "GRP010-4", "BOO021-1", "GEO002-4", "GRP128-4.003"]
      set0 = map (\n -> (n, extract nonequality n)) (prune src0)
      set1 = map (\n -> (n, extract equality n)) (prune src1)
      set2 = map (\n -> (n, extract tptp n)) (prune src2)
      program = "benchmark*"
  advertize (program ++ "==starting")
  advertize "settings"
  st <- readIORef settingsRef
  lim <- readIORef limitRef
  prn (showSettings st lim)
  withCnf True processSet ("nonequality", set0)
  withCnf True processSet ("equality", set1)
  withCnf False processSet ("tptp", set2)
  advertize (program ++ "==finishing")

-- The lines that do not depend on the CPU time (progress lines do)
stable :: String -> [String]
stable = filter (\l -> not ("[m(" `isPrefixOf` l) && not ("METIS: User" `isPrefixOf` l)) . lines

round' :: Int -> IO String
round' i = do
  writeIORef outBuf []
  mainOnce (vary i (nonequalityProblems, equalityProblems, tptpProblems) ([], [], []))
  concat . reverse <$> readIORef outBuf

main :: IO ()
main = do
  n <- iterations 1
  outs <- mapM round' [1 .. n]
  putStr (if all ((== stable (head outs)) . stable) outs then unlines (stable (last outs)) else "fail\n")

-- Problem1.nonequality (only the problems Main uses, in its order)
nonequalityProblems :: [(String, String)]
nonequalityProblems =
  [ ("P26", "\n((?x. p x) <=> ?x. q x) /\\ (!x y. p x /\\ q y ==> (r x <=> u y)) ==>\n((!x. p x ==> r x) <=> !x. q x ==> u x)")
  , ("P29", "\n(?x. p x) /\\ (?x. g x) ==>\n((!x. p x ==> h x) /\\ (!x. g x ==> j x) <=>\n !x y. p x /\\ g y ==> h x /\\ j y)")
  , ("P46", "\n(!x. p x /\\ (!y. p y /\\ h y x ==> g y) ==> g x) /\\\n((?x. p x /\\ ~g x) ==> ?x. p x /\\ ~g x /\\ !y. p y /\\ ~g y ==> j x y) /\\\n(!x y. p x /\\ p y /\\ h x y ==> ~j y x) ==> !x. p x ==> g x")
  , ("GILMORE_1", "\n?x. !y z.\n  (f y ==> g y <=> f x) /\\ (f y ==> h y <=> g x) /\\\n  ((f y ==> g y) ==> h y <=> h x) ==> f z /\\ g z /\\ h z")
  , ("LOS", "\n(!x y z. p x y ==> p y z ==> p x z) /\\\n(!x y z. q x y ==> q y z ==> q x z) /\\ (!x y. q x y ==> q y x) /\\\n(!x y. p x y \\/ q x y) ==> (!x y. p x y) \\/ !x y. q x y")
  , ("STEAM_ROLLER", "\n((!x. p1 x ==> p0 x) /\\ ?x. p1 x) /\\ ((!x. p2 x ==> p0 x) /\\ ?x. p2 x) /\\\n((!x. p3 x ==> p0 x) /\\ ?x. p3 x) /\\ ((!x. p4 x ==> p0 x) /\\ ?x. p4 x) /\\\n((!x. p5 x ==> p0 x) /\\ ?x. p5 x) /\\ ((?x. q1 x) /\\ !x. q1 x ==> q0 x) /\\\n(!x.\n   p0 x ==>\n   (!y. q0 y ==> r x y) \\/\n   !y. p0 y /\\ s0 y x /\\ (?z. q0 z /\\ r y z) ==> r x y) /\\\n(!x y. p3 y /\\ (p5 x \\/ p4 x) ==> s0 x y) /\\\n(!x y. p3 x /\\ p2 y ==> s0 x y) /\\ (!x y. p2 x /\\ p1 y ==> s0 x y) /\\\n(!x y. p1 x /\\ (p2 y \\/ q1 y) ==> ~r x y) /\\\n(!x y. p3 x /\\ p4 y ==> r x y) /\\ (!x y. p3 x /\\ p5 y ==> ~r x y) /\\\n(!x. p4 x \\/ p5 x ==> ?y. q0 y /\\ r x y) ==>\n?x y. p0 x /\\ p0 y /\\ ?z. q1 z /\\ r y z /\\ r x y")
  ]

-- Problem1.equality (only the problems Main uses, in its order)
equalityProblems :: [(String, String)]
equalityProblems =
  [ ("P48", "\n(a = b \\/ c = d) /\\ (a = c \\/ b = d) ==> a = d \\/ b = c")
  , ("P49", "\n(?x y. !z. z = x \\/ z = y) /\\ p a /\\ p b /\\ ~(a = b) ==> !x. p x")
  , ("AGATHA", "\n(?x. lives x /\\ killed x agatha) /\\\n(lives agatha /\\ lives butler /\\ lives charles) /\\\n(!x. lives x ==> x = agatha \\/ x = butler \\/ x = charles) /\\\n(!x y. killed x y ==> hates x y) /\\ (!x y. killed x y ==> ~richer x y) /\\\n(!x. hates agatha x ==> ~hates charles x) /\\\n(!x. ~(x = butler) ==> hates agatha x) /\\\n(!x. ~richer x agatha ==> hates butler x) /\\\n(!x. hates agatha x ==> hates butler x) /\\ (!x. ?y. ~hates x y) /\\\n~(agatha = butler) ==>\nkilled agatha agatha /\\ ~killed butler agatha /\\ ~killed charles agatha")
  ]

-- Problem1.tptp (only the problems Main uses, in its order)
tptpProblems :: [(String, String)]
tptpProblems =
  [ ("LCL009-1", "\n(!x y. ~p (x - y) \\/ ~p x \\/ p y) /\\\n(!x y z. p (x - y - (z - y - (x - z)))) ==>\n~p (a - b - c - (a - (b - c))) ==> F")
  , ("COL060-3", "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. b % x % y % z = x % (y % z)) /\\ (!x y. t % x % y = y % x) /\\\n(!x y z. ~(x = y) \\/ x % z = y % z) /\\\n(!x y z. ~(x = y) \\/ z % x = z % y) ==>\n~(b % (b % (t % b) % b) % t % c_x % c_y % c_z = c_y % (c_x % c_z)) ==> F")
  , ("COL058-2", "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y. r (r 0 x) y = r x (r y y)) /\\ (!x y z. ~(x = y) \\/ r x z = r y z) /\\\n(!x y z. ~(x = y) \\/ r z x = r z y) ==>\n~(r (r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0)))\n  (r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0))) =\n  r (r 0 (r (r 0 (r 0 0)) (r 0 (r 0 0)))) (r 0 (r 0 0))) ==> F")
  , ("LCL107-1", "\n(!x y. ~p (x - y) \\/ ~p x \\/ p y) /\\\n(!x y z v w x' y'.\n   p\n   (x - y - z - (v - w - (x' - w - (x' - v) - y')) -\n    (z - (y - x - y')))) ==> ~p (a - b - c - (e - b - (a - e - c))) ==> F")
  , ("LDA007-3", "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. f x (f y z) = f (f x y) (f x z)) /\\\n(!x y z. ~(x = y) \\/ f x z = f y z) /\\\n(!x y z. ~(x = y) \\/ f z x = f z y) /\\ tt = f t t /\\ ts = f t s /\\\ntt_ts = f tt ts /\\ tk = f t k /\\ tsk = f ts k ==>\n~(f t tsk = f tt_ts tk) ==> F")
  , ("GRP010-4", "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\ (!x y. ~(x = y) \\/ i x = i y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x y z. x * y * z = x * (y * z)) /\\\n(!x. 1 * x = x) /\\ (!x. i x * x = 1) /\\ c * b = 1 ==> ~(b * c = 1) ==> F")
  , ("BOO021-1", "\n(!x. x = x) /\\ (!x y. ~(x = y) \\/ y = x) /\\\n(!x y z. ~(x = y) \\/ ~(y = z) \\/ x = z) /\\\n(!x y z. ~(x = y) \\/ x + z = y + z) /\\\n(!x y z. ~(x = y) \\/ z + x = z + y) /\\ (!x y. ~(x = y) \\/ i x = i y) /\\\n(!x y z. ~(x = y) \\/ x * z = y * z) /\\\n(!x y z. ~(x = y) \\/ z * x = z * y) /\\ (!x y. (x + y) * y = y) /\\\n(!x y z. x * (y + z) = y * x + z * x) /\\ (!x. x + i x = 1) /\\\n(!x y. x * y + y = y) /\\ (!x y z. x + y * z = (y + x) * (z + x)) /\\\n(!x. x * i x = 0) ==> ~(b * a = a * b) ==> F")
  , ("GEO002-4", "\n(!x y z v. ~between x y z \\/ ~between y v z \\/ between x y v) /\\\n(!x y z. ~equidistant x y z z \\/ x == y) /\\\n(!x y z v w.\n   ~between x y z \\/ ~between v z w \\/\n   between x (outer_pasch y x v w z) v) /\\\n(!x y z v w.\n   ~between x y z \\/ ~between v z w \\/\n   between w y (outer_pasch y x v w z)) /\\\n(!x y z v. between x y (extension x y z v)) /\\\n(!x y z v. equidistant x (extension y x z v) z v) /\\\n(!x y z v. ~(x == y) \\/ ~between z v x \\/ between z v y) ==>\n~between a a b ==> F")
  , ("GRP128-4.003", "\n(!x y.\n   ~elt x \\/ ~elt y \\/ product e_1 x y \\/ product e_2 x y \\/\n   product e_3 x y) /\\\n(!x y.\n   ~elt x \\/ ~elt y \\/ product x e_1 y \\/ product x e_2 y \\/\n   product x e_3 y) /\\ elt e_1 /\\ elt e_2 /\\ elt e_3 /\\ ~(e_1 == e_2) /\\\n~(e_1 == e_3) /\\ ~(e_2 == e_1) /\\ ~(e_2 == e_3) /\\ ~(e_3 == e_1) /\\\n~(e_3 == e_2) /\\\n(!x y.\n   ~elt x \\/ ~elt y \\/ product x y e_1 \\/ product x y e_2 \\/\n   product x y e_3) /\\\n(!x y z v. ~product x y z \\/ ~product x y v \\/ z == v) /\\\n(!x y z v. ~product x y z \\/ ~product x v z \\/ y == v) /\\\n(!x y z v. ~product x y z \\/ ~product v y z \\/ x == v) ==>\n(!x y z v. product x y z \\/ ~product x z v \\/ ~product z y v) /\\\n(!x y z v. product x y z \\/ ~product v x z \\/ ~product v y x) /\\\n(!x y z v. ~product x y z \\/ ~product z y v \\/ product x z v) ==> F")
  ]
