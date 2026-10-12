-- tyan.sml (MLton benchmarks, 3cps test/programs/from-mlton; Thomas Yan,
-- via the TIL suite): a Groebner basis of the cyclic-6 ideal over F17,
-- with polynomials as term lists, intermediate sums in "geobuckets"
-- (mutable arrays of polynomials by size), and monomial ideals as tries in
-- refs.  Runs in IO.  The SML's global refs (statistics, counters) are
-- top-level IORefs; what it prints (progress per degree, then the basis)
-- goes to a per-round buffer, and one round's output is printed.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Array.IO
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Char (chr, ord)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Repeat

type Arr a = IOArray Int a

size1 :: Arr a -> IO Int
size1 a = (\(_, h) -> h + 1) <$> getBounds a

array0 :: IO (Arr [a])
array0 = newArray (0, -1) []

tabulate :: Int -> (Int -> a) -> IO (Arr a)
tabulate i f = if i <= 0 then error "Tabulate" else newListArray (0, i - 1) (map f [0 .. i - 1])

arrayoflist :: [a] -> IO (Arr a)
arrayoflist [] = error "ArrayofList"
arrayoflist l = newListArray (0, length l - 1) l

smlInt :: Int -> String
smlInt n = if n < 0 then '~' : show (negate n) else show n

-- What the program prints, this round
outBuf :: IORef [String]
outBuf = unsafePerformIO (newIORef [])
{-# NOINLINE outBuf #-}

prn :: String -> IO ()
prn s = modifyIORef outBuf (s :)

------------------------------------------------------------------------
-- Util

-- arr[i] := obj :: arr[i]; extend arr if necessary
utilInsert :: a -> Int -> Arr [a] -> IO (Arr [a])
utilInsert obj i arr = do
  len <- size1 arr
  if i < len
    then do { old <- readArray arr i; writeArray arr i (obj : old); return arr }
    else do
      arr' <- newArray (0, max (i + 1) (len + len) - 1) []
      forM_ [len - 1, len - 2 .. 0] $ \j -> readArray arr j >>= writeArray arr' j
      writeArray arr' i [obj]
      return arr'

-- The contents of a without duplicates (a is modified)
stripSort :: (a -> a -> Ordering) -> Arr a -> IO [a]
stripSort compare' a = size1 a >>= \n -> s 0 n []
  where
    swap i j = do { ai <- readArray a i; readArray a j >>= writeArray a i; writeArray a j ai }
    s i j acc = if i == j then return acc else do
      pivot <- readArray a ((i + j) `div` 2)
      let partition lo k hi = if k == hi then return (lo, hi) else do
            ak <- readArray a k
            case compare' pivot ak of
              LT -> swap lo k >> partition (lo + 1) (k + 1) hi
              EQ -> partition lo (k + 1) hi
              GT -> swap k (hi - 1) >> partition lo k (hi - 1)
      (lo, hi) <- partition i i j
      r <- s hi j acc
      s i lo (pivot : r)

------------------------------------------------------------------------
-- F: the field F17

p17 :: Int
p17 = 17

newtype F = F Int

fOne :: F
fOne = F 1

coerceInt :: Int -> F
coerceInt n = F (n `mod` p17)

fAdd, fSubtract, fMultiply :: F -> F -> F
fAdd (F n) (F m) = let k = n + m in if k >= p17 then F (k - p17) else F k
fSubtract (F n) (F m) = if n >= m then F (n - m) else F (n - m + p17)
fMultiply (F n) (F m) = F ((n * m) `mod` p17)

fNegate :: F -> F
fNegate (F 0) = F 0
fNegate (F n) = F (p17 - n)

reciprocal :: F -> F
reciprocal (F 0) = error "Div"
reciprocal (F n) = gcd' (p17, 0) (n, 1)
  where gcd' (a, a1) (b, b1) =
          if b == 1 then (if b1 < 0 then F (p17 + b1) else F b1)
          else let q = a `div` b in gcd' (b, b1) (a - q * b, a1 - q * b1)

fIsZero :: F -> Bool
fIsZero (F n) = n == 0

------------------------------------------------------------------------
-- M: monomials, (var, pwr) encoded as var << 16 + pwr

newtype M = M [Int]

mOne :: M
mOne = M []

xI :: Int -> M
xI v = M [(v `shiftL` 16) + 1]

mImplode :: [(Int, Int)] -> M
mImplode l = M (map (\(v, p) -> (v `shiftL` 16) + p) l)

mExplode :: M -> [(Int, Int)]
mExplode (M l) = map (\v -> (v `shiftR` 16, v .&. 65535)) l

mDeg :: M -> Int
mDeg (M l) = d l 0
  where d [] n = n
        d (u : ul) n = d ul ((u .&. 65535) + n)

mCompare :: M -> M -> Ordering
mCompare (M m) (M m') = cmp m m'
  where cmp [] [] = EQ
        cmp (_ : _) [] = GT
        cmp [] (_ : _) = LT
        cmp (u : us) (v : vs) = if u == v then cmp us vs else if u < v then LT else GT

mDisplay :: M -> String
mDisplay (M l) = foldl (flip d) [] l
  where dv v = if v < 26 then chr (v + ord 'a') else chr (v - 26 + ord 'A')
        d vv acc = let v = vv `shiftR` 16
                       p = vv .&. 65535
                   in if p == 1 then dv v : acc else dv v : (smlInt p ++ acc)

hiMask :: Int
hiMask = -65536

mMultiply :: M -> M -> M
mMultiply (M m0) (M m0') = M (mul m0 m0')
  where
    mul [] m = m
    mul m [] = m
    mul (u : us) (v : vs) =
      let uu = u .&. hiMask
      in if uu == (v .&. hiMask)
           then let w = u + (v .&. 65535)
                in if uu == (w .&. hiMask) then w : mul us vs
                   else error ("Mono.multiply overflow: " ++ mDisplay (M (u : us)) ++ ", " ++ mDisplay (M (v : vs)))
           else if u > v then u : mul us (v : vs)
           else v : mul (u : us) vs

mLcm :: M -> M -> M
mLcm (M m0) (M m0') = M (lcm' m0 m0')
  where
    lcm' [] m = m
    lcm' m [] = m
    lcm' (u : us) (v : vs) =
      if u >= v then if (u .&. hiMask) < v then u : lcm' us vs else u : lcm' us (v : vs)
      else if (v .&. hiMask) < u then v : lcm' us vs else v : lcm' (u : us) vs

tryDivide :: M -> M -> Maybe M
tryDivide (M m0) (M m0') = d m0 m0' []
  where
    rev [] l = l
    rev (x : xs) l = rev xs (x : l)
    d m [] q = Just (M (rev q m))
    d [] (_ : _) _ = Nothing
    d (u : us) (v : vs) q
      | u < v = Nothing
      | (u .&. hiMask) == (v .&. hiMask) = if u == v then d us vs q else d us vs (u - (v .&. 65535) : q)
      | otherwise = d us (v : vs) (u : q)

mDivide :: M -> M -> M
mDivide m m' = case tryDivide m m' of
  Just q -> q
  Nothing -> error "DoesntDivide"

------------------------------------------------------------------------
-- MI: monomial ideals, as tries indexed by increasing vars, children in
-- increasing degree order, and the least degree above all elements

data MT a = MT (Maybe a) [(Int, MT a)]
newtype MI a = MI (IORef (Int, MT a))

emptyTrie :: MT a
emptyTrie = MT Nothing []

mkEmpty :: IO (MI a)
mkEmpty = MI <$> newIORef (0, emptyTrie)

encode :: (Int, Int) -> Int
encode (var, pwr) = (var `shiftL` 16) + pwr

grabVar, grabPwr :: Int -> Int
grabVar vp = vp .&. hiMask
grabPwr vp = vp .&. 65535

smallerVar :: Int -> Int -> Bool
smallerVar vp vp' = vp < grabVar vp'

-- The first element dividing m', and its tag
search :: MI a -> M -> IO (Maybe (M, a))
search (MI x) (M m') = do
  (_, mt) <- readIORef x
  let s _ m (MT (Just a) _) = Just (M m, a)
      s m'' m (MT Nothing trie) = s' m'' m trie
      s' [] _ _ = Nothing
      s' _ _ [] = Nothing
      s' (vp' : m'') m trie@((vp, child) : children)
        | smallerVar vp' vp = s' m'' m trie
        | grabPwr vp == 0 = s (vp' : m'') m child <|> s' (vp' : m'') m children
        | smallerVar vp vp' = Nothing
        | vp <= vp' = s m'' (vp : m) child <|> s' (vp' : m'') m children
        | otherwise = Nothing
  return (s (reverse m') [] mt)

-- m must be a new generator, not a multiple of an existing one
miInsert :: MI a -> M -> a -> IO ()
miInsert (MI mi) m a = do
  (d, mt) <- readIORef mi
  let i [] (MT (Just _) _) = error "MONO_IDEAL.insert duplicate"
      i [] (MT Nothing children) = MT (Just a) children
      i (vp : m') (MT a' []) = MT a' [(vp, i m' emptyTrie)]
      i (vp : m') mt'@(MT a' trie@((vp', _) : _))
        | smallerVar vp vp' = MT a' [(grabVar vp, MT Nothing trie), (vp, i m' emptyTrie)]
        | smallerVar vp' vp = i (grabVar vp' : vp : m') mt'
        | otherwise = MT a' (j trie)
        where j [] = [(vp, i m' emptyTrie)]
              j ((vp'', child) : children)
                | vp < vp'' = (vp, i m' emptyTrie) : (vp'', child) : children
                | vp == vp'' = (vp'', i m' child) : children
                | otherwise = (vp'', child) : j children
  writeIORef mi (max d (mDeg m), i (reverse (map encode (mExplode m))) mt)

mkIdeal :: [(M, a)] -> IO (MI a)
mkIdeal [] = mkEmpty
mkIdeal origMs = do
  msa <- arrayoflist origMs
  ms <- stripSort (\(m, _) (m', _) -> mCompare m m') msa
  let ins [] = array0
      ins (x@(m, _) : xs) = ins xs >>= utilInsert x (mDeg m)
  buckets <- ins ms
  n <- size1 buckets
  mi <- mkEmpty
  let sort i = if i >= n then return mi else do
        let redundant (m, _) = (\r -> case r of { Nothing -> False; Just _ -> True }) <$> search mi m
            filter' [] l = mapM_ (\(m, a) -> miInsert mi m a) l
            filter' (x : xx) l = redundant x >>= \r -> if r then filter' xx l else filter' xx (x : l)
        readArray buckets i >>= \b -> filter' b []
        writeArray buckets i []
        sort (i + 1)
  sort 0

miFold :: ((M, a) -> b -> IO b) -> MI a -> b -> IO b
miFold g (MI x) initial = do
  (_, mt) <- readIORef x
  let f acc m (MT Nothing children) = f' acc m children
      f acc m (MT (Just a) children) = g (M m, a) acc >>= \acc' -> f' acc' m children
      f' acc _ [] = return acc
      f' acc m ((vp, child) : children) =
        if grabPwr vp == 0 then f acc m child >>= \acc' -> f' acc' m children
        else f acc (vp : m) child >>= \acc' -> f' acc' m children
  f initial [] mt

------------------------------------------------------------------------
-- Statistics on the sizes of added polynomials (globals in the SML)

logPair :: Int -> Int
logPair n0 = lg n0 0
  where lg n l = if n <= 1 then l else lg (n `shiftR` 1) (1 + l)

maxLeft, maxRight :: IORef Int
maxLeft = unsafePerformIO (newIORef 0)
{-# NOINLINE maxLeft #-}
maxRight = unsafePerformIO (newIORef 0)
{-# NOINLINE maxRight #-}

counts :: Arr (IOUArray Int Int)
counts = unsafePerformIO (mapM (\_ -> newArray (0, 19) 0) [1 .. 20 :: Int] >>= newListArray (0, 19))
{-# NOINLINE counts #-}

pair :: Int -> Int -> IO ()
pair l0 r0 = do
  let l = logPair l0
      r = logPair r0
  modifyIORef' maxLeft (max l)
  modifyIORef' maxRight (max r)
  a <- readArray counts l
  readArray a r >>= writeArray a r . (+ 1)

------------------------------------------------------------------------
-- P: polynomials, terms in descending monomial order

newtype P = P [(F, M)]

pZero :: P
pZero = P []

pCoerce :: (F, M) -> P
pCoerce am = P [am]

pCons :: (F, M) -> P -> P
pCons am (P p) = P (am : p)

plus :: [(F, M)] -> [(F, M)] -> [(F, M)]
plus [] p2 = p2
plus p1 [] = p1
plus ((a, m) : ms) ((b, n) : ns) = case mCompare m n of
  LT -> (b, n) : plus ((a, m) : ms) ns
  GT -> (a, m) : plus ms ((b, n) : ns)
  EQ -> let c = fAdd a b in if fIsZero c then plus ms ns else (c, m) : plus ms ns

minus :: [(F, M)] -> [(F, M)] -> [(F, M)]
minus [] p2 = map (\(a, m) -> (fNegate a, m)) p2
minus p1 [] = p1
minus ((a, m) : ms) ((b, n) : ns) = case mCompare m n of
  LT -> (fNegate b, n) : minus ((a, m) : ms) ns
  GT -> (a, m) : minus ms ((b, n) : ns)
  EQ -> let c = fSubtract a b in if fIsZero c then minus ms ns else (c, m) : minus ms ns

termMult' :: F -> M -> [(F, M)] -> [(F, M)]
termMult' a m p = map (\(a', m') -> (fMultiply a a', mMultiply m m')) p

pAdd, pSubtract :: P -> P -> IO P
pAdd (P p1) (P p2) = pair (length p1) (length p2) >> return (P (plus p1 p2))
pSubtract (P p1) (P p2) = pair (length p1) (length p2) >> return (P (minus p1 p2))

spair :: F -> M -> P -> F -> M -> P -> IO P
spair a m (P f) b n (P g) = pair (length f) (length g) >> return (P (minus (termMult' a m f) (termMult' b n g)))

pTermMult :: F -> M -> P -> P
pTermMult a m (P f) = P (termMult' a m f)

scalarMult :: F -> P -> P
scalarMult a (P p) = P (map (\(b, m) -> (fMultiply a b, m)) p)

pIsZero :: P -> Bool
pIsZero (P []) = True
pIsZero (P (_ : _)) = False

leadMono :: P -> M
leadMono (P ((_, m) : _)) = m
leadMono (P []) = error "POLY.leadMono"

leadCoeff :: P -> F
leadCoeff (P ((a, _) : _)) = a
leadCoeff (P []) = error "POLY.leadCoeff"

pRest :: P -> P
pRest (P (_ : p)) = P p
pRest (P []) = error "POLY.rest"

pLeadAndRest :: P -> ((F, M), P)
pLeadAndRest (P (lead : rest)) = (lead, P rest)
pLeadAndRest (P []) = error "POLY.leadAndRest"

pDeg :: P -> Int
pDeg (P []) = error "POLY.deg on zero poly"
pDeg (P ((_, m) : _)) = mDeg m

numTerms :: P -> Int
numTerms (P p) = length p

------------------------------------------------------------------------
-- HP: geobuckets, polynomials of size about 4^i in bucket i

newtype HP = HP (Arr P)

hpLog :: Int -> Int
hpLog n0 = lg n0 0
  where lg n l = if n < 8 then l else lg (n `shiftR` 2) (1 + l)

mkHPoly :: P -> IO HP
mkHPoly p = let l = hpLog (numTerms p) in HP <$> tabulate (l + 1) (\i -> if i == l then p else pZero)

hpAdd :: P -> HP -> IO HP
hpAdd p (HP ps) = do
  let l = hpLog (numTerms p)
  n <- size1 ps
  if l >= n
    then do
      old <- getElems ps
      HP <$> tabulate (n + n) (\i -> if i < n then old !! i else if i == l then p else pZero)
    else do
      p' <- readArray ps l >>= pAdd p
      if l == hpLog (numTerms p')
        then writeArray ps l p' >> return (HP ps)
        else writeArray ps l pZero >> hpAdd p' (HP ps)

hpLeadAndRest :: HP -> IO (Maybe (F, M, HP))
hpLeadAndRest (HP ps) = do
  n <- size1 ps
  let lar m indices i = if i >= n then lar' m indices else do
        p <- readArray ps i
        if pIsZero p then lar m indices (i + 1)
        else if null indices then lar (leadMono p) [i] (i + 1)
        else case mCompare m (leadMono p) of
          LT -> lar (leadMono p) [i] (i + 1)
          EQ -> lar m (i : indices) (i + 1)
          GT -> lar m indices (i + 1)
      lar' _ [] = return Nothing
      lar' m (i : is) = do
        let extract j = do
              ((a, _), rest) <- pLeadAndRest <$> readArray ps j
              writeArray ps j rest
              return a
            revfold [] b = return b
            revfold (j : js) b = revfold js b >>= \b' -> extract j >>= \a -> return (fAdd a b')
        a <- extract i >>= revfold is
        if fIsZero a then lar mOne [] 0 else return (Just (a, m, HP ps))
  lar mOne [] 0

------------------------------------------------------------------------
-- G: the Groebner basis (counters are globals in the SML)


maybePairs, primePairs, usedPairs, newGens, maxDeg :: IORef Int
maybePairs = unsafePerformIO (newIORef 0)
{-# NOINLINE maybePairs #-}
primePairs = unsafePerformIO (newIORef 0)
{-# NOINLINE primePairs #-}
usedPairs = unsafePerformIO (newIORef 0)
{-# NOINLINE usedPairs #-}
newGens = unsafePerformIO (newIORef 0)
{-# NOINLINE newGens #-}
maxDeg = unsafePerformIO (newIORef 1000000)
{-# NOINLINE maxDeg #-}

autoReduceOn :: IORef Bool
autoReduceOn = unsafePerformIO (newIORef True)
{-# NOINLINE autoReduceOn #-}

resetG :: IO ()
resetG = mapM_ (`writeIORef` 0) [maybePairs, primePairs, usedPairs, newGens]

inc :: IORef Int -> IO ()
inc r = modifyIORef' r (+ 1)

reduce :: P -> MI (IORef P) -> IO P
reduce f mi = if pIsZero f then return f else P <$> (mkHPoly f >>= r)
  where
    r hp = hpLeadAndRest hp >>= \lr -> case lr of
      Nothing -> return []
      Just (a, m, hp') -> search mi m >>= \s -> case s of
        Nothing -> ((a, m) :) <$> r hp'
        Just (m', p) -> do
          pv <- readIORef p
          hpAdd (pTermMult (fNegate a) (mDivide m m') pv) hp' >>= r

mkMonic :: P -> P
mkMonic f = scalarMult (reciprocal (leadCoeff f)) f

-- New s-pairs from monic h and the generators in mi, into degree buckets
addPairs :: P -> MI (IORef P) -> Arr [Arr P] -> IO (Arr [Arr P])
addPairs h mi pairs = do
  let m = leadMono h
      d = mDeg m
      tag (m', g') quots = do
        inc maybePairs
        g <- readIORef g'
        return ((mDivide (mLcm m m') m, (m', g)) : quots)
      insert (mm, (m', g')) arr =
        if case mCompare m' mm of { EQ -> True; _ -> False }
          then inc primePairs >> return arr
          else inc usedPairs >> utilInsert (pCons (fOne, m') g') (mDeg mm + d) arr
  quots <- miFold tag mi []
  ideal <- mkIdeal quots
  buckets <- array0 >>= miFold insert ideal
  let ins (-1) ps = return ps
      ins i ps = readArray buckets i >>= \b -> case b of
        [] -> ins (i - 1) ps
        gs -> arrayoflist (h : gs) >>= \a -> utilInsert a i ps >>= ins (i - 1)
  n <- size1 buckets
  ins (n - 1) pairs

grobner :: [P] -> IO (MI (IORef P))
grobner fs0 = do
  let pr l = prn (concat (l ++ ["\n"]))
      insFs [] = array0
      insFs (f : rest) = insFs rest >>= utilInsert f (pDeg f)
  fs <- insFs fs0
  nfs <- size1 fs
  pairs <- newArray (0, nfs - 1) [] >>= newIORef
  mi <- mkEmpty
  newDegGens <- newIORef []
  autoRed <- readIORef autoReduceOn
  let addGen h =
        if not autoRed
          then newIORef (pRest h) >>= miInsert mi (leadMono h)
          else do
            let ((_, m), rh) = pLeadAndRest h
                autoReduce f =
                  if pIsZero f then return f
                  else let ((a, m'), rf) = pLeadAndRest f
                       in case mCompare m m' of
                            LT -> pCons (a, m') <$> autoReduce rf
                            EQ -> pSubtract rf (scalarMult a rh)
                            GT -> return f
            rrh <- newIORef rh
            miInsert mi (leadMono h) rrh
            readIORef newDegGens >>= mapM_ (\f -> readIORef f >>= autoReduce >>= writeIORef f)
            modifyIORef newDegGens (rrh :)
  tasksleft <- newIORef (0 :: Int)
  let feedback = do
        n <- readIORef tasksleft
        when ((n .&. 15) == 0) (prn (smlInt n))
        prn "."
        writeIORef tasksleft (n - 1)
      try' h0 = do
        feedback
        h <- reduce h0 mi
        if pIsZero h then return () else do
          let h' = mkMonic h
          prn "#"
          readIORef pairs >>= addPairs h' mi >>= writeIORef pairs
          addGen h'
          inc newGens
      tryPairs fgs = do
        ((a, m), f) <- pLeadAndRest <$> readArray fgs 0
        let tryPair i = if i == 0 then return () else do
              ((b, n), g) <- pLeadAndRest <$> readArray fgs i
              let k = mLcm m n
              spair b (mDivide k m) f a (mDivide k n) g >>= try'
              tryPair (i - 1)
        size1 fgs >>= \l -> tryPair (l - 1)
      numPairs [] n = return n
      numPairs (p : ps) n = size1 p >>= \l -> numPairs ps (n - 1 + l)
      gb d = do
        ps <- readIORef pairs
        lp <- size1 ps
        if d >= lp then return mi else do
          pd <- readArray ps d
          np <- numPairs pd 0
          gens <- if d >= nfs then return "0" else show . length <$> readArray fs d
          pr ["DEGREE ", smlInt d, " with ", smlInt np, " pairs ", gens, " generators to do"]
          writeIORef tasksleft np
          when (d < nfs) $ readArray fs d >>= \l -> modifyIORef tasksleft (+ length l)
          md <- readIORef maxDeg
          when (d <= md) $ do
            resetG
            writeIORef newDegGens []
            mapM_ tryPairs pd
            readIORef pairs >>= \ps' -> writeArray ps' d []
            when (d < nfs) $ do
              readArray fs d >>= mapM_ try'
              writeArray fs d []
            [mp, pp, up, ng] <- mapM readIORef [maybePairs, primePairs, usedPairs, newGens]
            pr ["maybe ", smlInt mp, " prime ", smlInt pp, " using ", smlInt up, "; found ", smlInt ng]
          gb (d + 1)
  gb 0

------------------------------------------------------------------------
-- Parsing polynomials

data Ch = Dig Int | Var Int | Sign Int

char' :: Char -> Ch
char' ch
  | ord '0' <= och && och <= ord '9' = Dig (och - ord '0')
  | ord 'a' <= och && och <= ord 'z' = Var (och - ord 'a')
  | ord 'A' <= och && och <= ord 'Z' = Var (och - ord 'A' + 26)
  | och == ord '+' = Sign 1
  | och == ord '-' = Sign (-1)
  | otherwise = error ("bad ch in poly: " ++ [ch])
  where och = ord ch

nat :: Int -> [Ch] -> (Int, [Ch])
nat n (Dig d : l) = nat (n * 10 + d) l
nat n l = (n, l)

mono :: M -> [Ch] -> (M, [Ch])
mono m (Var v : Dig d : l) = let (n, l') = nat d l in mono (mMultiply (mImplode [(v, n)]) m) l'
mono m (Var v : l) = mono (mMultiply (xI v) m) l
mono m l = (m, l)

term :: [Ch] -> ((F, M), [Ch])
term l0 =
  let (n, l1) = case l0 of { (Dig d : l) -> nat d l; _ -> (1, l0) }
      (m, l2) = mono mOne l1
  in ((coerceInt n, m), l2)

poly :: P -> [Ch] -> IO P
poly p [] = return p
poly p l0 = do
  let (s, l1) = case l0 of { (Sign s' : l) -> (coerceInt s', l); _ -> (fOne, l0) }
      ((a, m), l2) = term l1
  p' <- pAdd (pCoerce (fMultiply s a, m)) p
  poly p' l2

parsePoly :: String -> IO P
parsePoly s = poly pZero (map char' s)

------------------------------------------------------------------------

grab :: MI (IORef P) -> IO [P]
grab mi = miFold (\(m, g) l -> readIORef g >>= \gv -> return (pCons (fOne, m) gv : l)) mi []

gb :: [P] -> IO ()
gb fs0 = do
  g <- grobner fs0
  fs <- grab g
  forM_ fs $ \f -> prn (mDisplay (leadMono f) ++ " + " ++ smlInt (numTerms f - 1) ++ " terms\n")

u6 :: [String]
u6 = ["abcdef-g6", "a+b+c+d+e+f", "ab+bc+cd+de+ef+fa", "abc+bcd+cde+def+efa+fab",
      "abcd+bcde+cdef+defa+efab+fabc", "abcde+bcdef+cdefa+defab+efabc+fabcd"]

round' :: Int -> IO String
round' i = do
  writeIORef outBuf []
  mapM parsePoly (vary i u6 []) >>= gb
  concat . reverse <$> readIORef outBuf

main :: IO ()
main = do
  n <- iterations 12
  outs <- mapM round' [1 .. n]
  putStr (if all (== head outs) outs then head outs else "fail\n")
