-- sorts.ml (OCaml testsuite tests/misc), in its default mode: tests the
-- OCaml 4.14 standard library's List.sort, List.stable_sort (a merge sort
-- that alternates between ascending and descending runs), Array.sort (a
-- ternary heap sort) and Array.stable_sort (a merge sort with insertion
-- sort below 5 elements) on ints and records, at 25 sizes up to 2323,
-- checking that each output is sorted and a permutation of the input (and,
-- for the stable sorts, stable).  The other sorts in sorts.ml are reachable
-- only through its command-line flags and are not translated.  OCaml's
-- Random (seeded through MD5) is replaced by an LCG: the output depends
-- only on the sizes and the checks.  Runs in IO: the arrays are mutable,
-- as in the OCaml.  Prints the same lines as the OCaml.
module Main (main) where

import Control.Exception
import Control.Monad (forM_, when, unless)
import Data.Array.IO
import Data.Bits (shiftR, (.&.))
import Data.IORef
import Repeat

-- State of one round: the random generator, the output, the failure count
data Env = Env { rng :: IORef Int, out :: IORef [String], numFailed :: IORef Int }

printf' :: Env -> String -> IO ()
printf' env s = modifyIORef out' (s :)
  where out' = out env

-- Random numbers (30 bits, as OCaml's Random.bits)
bits :: Env -> IO Int
bits env = do
  modifyIORef' (rng env) (\s -> s * 6364136223846793005 + 1442695040888963407)
  s <- readIORef (rng env)
  return ((s `shiftR` 34) .&. 0x3FFFFFFF)

randomInt :: Env -> Int -> IO Int
randomInt env n = (`mod` n) <$> bits env

randomReinit :: Env -> IO ()
randomReinit env = writeIORef (rng env) 0

-- A state to replay the numbers that follow it
randomGetState :: Env -> IO Int
randomGetState env = bits env >> readIORef (rng env)

randomSetState :: Env -> Int -> IO ()
randomSetState env = writeIORef (rng env)

type Arr e = IOArray Int e

fromList :: [e] -> IO (Arr e)
fromList xs = newListArray (0, length xs - 1) xs

len :: Arr e -> IO Int
len a = (\(_, h) -> h + 1) <$> getBounds a

cmpInt :: Int -> Int -> Int
cmpInt x y = fromEnum (compare x y) - 1

------------------------------------------------------------------------
-- The data and their checks

mkconst, mksorted, mkrev :: Env -> Int -> IO (Arr Int)
mkconst _ n = fromList (replicate n 0)
mksorted _ n = fromList [0 .. n - 1]
mkrev _ n = fromList [n - 1, n - 2 .. 0]

chkconst, chksorted, chkrev :: Env -> Int -> Int -> Arr Int -> IO Bool
chkconst _ _ n a = (== replicate n 0) <$> getElems a
chksorted _ _ n a = (== [0 .. n - 1]) <$> getElems a
chkrev = chksorted

data Exit = Exit deriving Show
instance Exception Exit

-- Out of bounds is Invalid_argument in the OCaml; both make the check fail
(!.) :: Arr e -> Int -> IO e
a !. i = do
  (_, h) <- getBounds a
  if i < 0 || i > h then throwIO Exit else readArray a i

-- Is a sorted, and does it hold the elements mke makes after rstate?
chkgen :: Eq e => Env -> (Int -> IO e) -> (e -> e -> Int) -> Int -> Int -> Arr e -> IO Bool
chkgen env mke cmp rstate n a = (body >> return True) `catch` \Exit -> return False
  where
    body = do
      marks <- newArray (0, n - 1) (-1) :: IO (IOUArray Int Int)
      let rdm i = if i < 0 || i >= n then throwIO Exit else readArray marks i
          skipmarks l = do
            ml <- rdm l
            if ml == -1 then return l else do
              let go m = rdm m >>= \mm -> if mm /= -1 then go (m + 1) else return m
              m <- go ml
              writeArray marks l m
              return m
          linear e l0 = skipmarks l0 >>= loop
            where loop l = do
                    x <- a !. l
                    if cmp x e > 0 then throwIO Exit
                    else if e == x then (if l < n then writeArray marks l (l + 1) else throwIO Exit)
                    else loop (l + 1)
          dicho e l r
            | l == r = linear e l
            | otherwise = do
                let m = (l + r) `div` 2
                x <- a !. m
                if cmp x e >= 0 then dicho e l m else dicho e (m + 1) r
      forM_ [0 .. n - 2] $ \i -> do
        x <- a !. i
        y <- a !. (i + 1)
        when (cmp x y > 0) (throwIO Exit)
      randomSetState env rstate
      la <- len a
      forM_ [0 .. n - 1] $ \i -> mke i >>= \e -> dicho e 0 (la - 1)

mkrandDup, mkrandNodup :: Env -> Int -> IO (Arr Int)
mkrandDup env n = mapM (\_ -> randomInt env n) [1 .. n] >>= fromList
mkrandNodup env n = mapM (\_ -> bits env) [1 .. n] >>= fromList

chkrandDup, chkrandNodup :: Env -> Int -> Int -> Arr Int -> IO Bool
chkrandDup env rstate n = chkgen env (\_ -> randomInt env n) cmpInt rstate n
chkrandNodup env rstate n = chkgen env (\_ -> bits env) cmpInt rstate n

data Record = Record { s1 :: String, s2 :: String, i1 :: Int, i2 :: Int } deriving Eq

randString :: Env -> IO String
randString env = do
  l <- randomInt env 10
  mapM (\_ -> toEnum <$> randomInt env 256) [1 .. l]

mkrec1 :: Env -> Int -> Int -> IO Record
mkrec1 env b i = do
  a <- randString env
  c <- randString env
  r <- randomInt env b
  return (Record a c r i)

mkrecs :: Env -> Int -> Int -> IO (Arr Record)
mkrecs env b n = mapM (mkrec1 env b) [0 .. n - 1] >>= fromList

cmpstr, cmpint, cmplex :: Record -> Record -> Int
cmpstr r1 r2 = let c = fromEnum (compare (s1 r1) (s1 r2)) - 1 in if c == 0 then fromEnum (compare (s2 r1) (s2 r2)) - 1 else c
cmpint r1 r2 = cmpInt (i1 r1) (i1 r2)
cmplex r1 r2 = let c = cmpInt (i1 r1) (i1 r2) in if c == 0 then cmpInt (i2 r1) (i2 r2) else c

chkstr, chkint, chklex :: Env -> Int -> Int -> Int -> Arr Record -> IO Bool
chkstr env b = chkgen env (mkrec1 env b) cmpstr
chkint env b = chkgen env (mkrec1 env b) cmpint
chklex env b = chkgen env (mkrec1 env b) cmplex

lens :: Int -> [Int]
lens k = map (+ k) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 28, 100, 127, 128, 129, 193, 506, 1000, 1025, 1535, 2323]

------------------------------------------------------------------------
-- The sorts (OCaml 4.14's stdlib)

listStableSort :: (e -> e -> Int) -> [e] -> [e]
listStableSort cmp l = if n < 2 then l else fst (sort n l)
  where
    n = length l
    revAppend xs acc = foldl (flip (:)) acc xs
    revMerge [] l2 acc = revAppend l2 acc
    revMerge l1 [] acc = revAppend l1 acc
    revMerge l1@(h1 : t1) l2@(h2 : t2) acc =
      if cmp h1 h2 <= 0 then revMerge t1 l2 (h1 : acc) else revMerge l1 t2 (h2 : acc)
    revMergeRev [] l2 acc = revAppend l2 acc
    revMergeRev l1 [] acc = revAppend l1 acc
    revMergeRev l1@(h1 : t1) l2@(h2 : t2) acc =
      if cmp h1 h2 > 0 then revMergeRev t1 l2 (h1 : acc) else revMergeRev l1 t2 (h2 : acc)
    sort 2 (x1 : x2 : tl) = (if cmp x1 x2 <= 0 then [x1, x2] else [x2, x1], tl)
    sort 3 (x1 : x2 : x3 : tl) =
      ( if cmp x1 x2 <= 0 then
          if cmp x2 x3 <= 0 then [x1, x2, x3]
          else if cmp x1 x3 <= 0 then [x1, x3, x2]
          else [x3, x1, x2]
        else if cmp x1 x3 <= 0 then [x2, x1, x3]
        else if cmp x2 x3 <= 0 then [x2, x3, x1]
        else [x3, x2, x1]
      , tl )
    sort k xs =
      let k1 = k `shiftR` 1
          (ys1, l2) = revSort k1 xs
          (ys2, tl) = revSort (k - k1) l2
      in (revMergeRev ys1 ys2 [], tl)
    revSort 2 (x1 : x2 : tl) = (if cmp x1 x2 > 0 then [x1, x2] else [x2, x1], tl)
    revSort 3 (x1 : x2 : x3 : tl) =
      ( if cmp x1 x2 > 0 then
          if cmp x2 x3 > 0 then [x1, x2, x3]
          else if cmp x1 x3 > 0 then [x1, x3, x2]
          else [x3, x1, x2]
        else if cmp x1 x3 > 0 then [x2, x1, x3]
        else if cmp x2 x3 > 0 then [x2, x3, x1]
        else [x3, x2, x1]
      , tl )
    revSort k xs =
      let k1 = k `shiftR` 1
          (ys1, l2) = sort k1 xs
          (ys2, tl) = sort (k - k1) l2
      in (revMerge ys1 ys2 [], tl)

listSort :: (e -> e -> Int) -> [e] -> [e]
listSort = listStableSort

arraySort :: (e -> e -> Int) -> Arr e -> IO ()
arraySort cmp a = do
  l <- len a
  let get = readArray a
      set = writeArray a
      -- Nothing: the OCaml's Bottom
      maxson l' i = do
        let i31 = i + i + i + 1
        if i31 + 2 < l' then do
          x0 <- get i31; x1 <- get (i31 + 1)
          let x = if cmp x0 x1 < 0 then i31 + 1 else i31
          y <- get x; x2 <- get (i31 + 2)
          return (Just (if cmp y x2 < 0 then i31 + 2 else x))
        else if i31 + 1 < l' then do
          x0 <- get i31; x1 <- get (i31 + 1)
          return (Just (if cmp x0 x1 < 0 then i31 + 1 else i31))
        else return (if i31 < l' then Just i31 else Nothing)
      trickle l' i e = maxson l' i >>= \mj -> case mj of
        Nothing -> set i e
        Just j -> do
          aj <- get j
          if cmp aj e > 0 then set i aj >> trickle l' j e else set i e
      bubble l' i = maxson l' i >>= \mj -> case mj of
        Nothing -> return i
        Just j -> get j >>= set i >> bubble l' j
      trickleup i e = do
        let father = (i - 1) `quot` 3
        af <- get father
        if cmp af e < 0
          then do
            set i af
            if father > 0 then trickleup father e else set 0 e
          else set i e
  forM_ [(l + 1) `quot` 3 - 1, (l + 1) `quot` 3 - 2 .. 0] $ \i -> get i >>= trickle l i
  forM_ [l - 1, l - 2 .. 2] $ \i -> do
    e <- get i
    get 0 >>= set i
    j <- bubble i 0
    trickleup j e
  when (l > 1) $ do
    e <- get 1
    get 0 >>= set 1
    set 0 e

-- Array.blit: overlapping ranges copy as if through a temporary
blit :: Arr e -> Int -> Arr e -> Int -> Int -> IO ()
blit src so dst d n
  | so >= d = forM_ [0 .. n - 1] $ \i -> readArray src (so + i) >>= writeArray dst (d + i)
  | otherwise = forM_ [n - 1, n - 2 .. 0] $ \i -> readArray src (so + i) >>= writeArray dst (d + i)

cutoff :: Int
cutoff = 5

arrayStableSort :: (e -> e -> Int) -> Arr e -> IO ()
arrayStableSort cmp a = do
  let merge src1ofs src1len src2 src2ofs src2len dst dstofs = do
        let src1r = src1ofs + src1len
            src2r = src2ofs + src2len
            loop i1' s1' i2' s2' d =
              if cmp s1' s2' <= 0
                then do
                  writeArray dst d s1'
                  let i1'' = i1' + 1
                  if i1'' < src1r then readArray a i1'' >>= \x -> loop i1'' x i2' s2' (d + 1)
                  else blit src2 i2' dst (d + 1) (src2r - i2')
                else do
                  writeArray dst d s2'
                  let i2'' = i2' + 1
                  if i2'' < src2r then readArray src2 i2'' >>= \x -> loop i1' s1' i2'' x (d + 1)
                  else blit a i1' dst (d + 1) (src1r - i1')
        x <- readArray a src1ofs
        y <- readArray src2 src2ofs
        loop src1ofs x src2ofs y dstofs
      isortto srcofs dst dstofs l =
        forM_ [0 .. l - 1] $ \i -> do
          e <- readArray a (srcofs + i)
          let shift j = if j >= dstofs
                then readArray dst j >>= \dj ->
                       if cmp dj e > 0 then writeArray dst (j + 1) dj >> shift (j - 1) else return j
                else return j
          j <- shift (dstofs + i - 1)
          writeArray dst (j + 1) e
      sortto srcofs dst dstofs l =
        if l <= cutoff then isortto srcofs dst dstofs l else do
          let l1 = l `quot` 2
              l2 = l - l1
          sortto (srcofs + l1) dst (dstofs + l1) l2
          sortto srcofs a (srcofs + l2) l1
          merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs
  l <- len a
  if l <= cutoff then isortto 0 a 0 l else do
    let l1 = l `quot` 2
        l2 = l - l1
    a0 <- readArray a 0
    t <- newArray (0, l2 - 1) a0
    sortto l1 t 0 l2
    sortto 0 a l2 l1
    merge l2 l1 t 0 l2 a 0

------------------------------------------------------------------------
-- The tests

-- A sort run on an array: the array of its output
newtype Sorter = Sorter (forall e. (e -> e -> Int) -> Arr e -> IO (Arr e))

onList :: (forall e. (e -> e -> Int) -> [e] -> [e]) -> Sorter
onList f = Sorter (\cmp a -> getElems a >>= fromList . f cmp)

onArray :: (forall e. (e -> e -> Int) -> Arr e -> IO ()) -> Sorter
onArray f = Sorter (\cmp a -> f cmp a >> return a)

test1 :: Env -> Int -> String -> Sorter -> (e -> e -> Int) -> String
      -> (Int -> IO (Arr e)) -> (Int -> Int -> Arr e -> IO Bool) -> IO ()
test1 env k name (Sorter f) cmp desc mk chk = do
  randomReinit env
  printf' env ("  " ++ name ++ " with " ++ desc)
  col <- newIORef (0 :: Int)
  forM_ (lens k) $ \n -> do
    i <- readIORef col
    when (i == 0) (printf' env "\n    ")
    writeIORef col (if i + 1 > 11 then 0 else i + 1)
    let s = show n in printf' env (replicate (5 - length s) ' ' ++ s)
    rstate <- randomGetState env
    a <- mk n
    v <- f cmp a
    printf' env "."
    ok <- chk rstate n v
    unless ok $ modifyIORef' (numFailed env) (+ 1) >> printf' env "\n*** FAIL\n"
  printf' env "\n"

test :: Env -> Int -> String -> Bool -> Sorter -> IO ()
test env k name stable f = do
  printf' env ("Testing " ++ name ++ "...\n")
  let t :: (e -> e -> Int) -> String -> (Env -> Int -> IO (Arr e)) -> (Env -> Int -> Int -> Arr e -> IO Bool) -> IO ()
      t cmp desc mk chk = test1 env k name f cmp desc (mk env) (chk env)
  t cmpInt "constant ints" mkconst chkconst
  t cmpInt "sorted ints" mksorted chksorted
  t cmpInt "reverse-sorted ints" mkrev chkrev
  t cmpInt "random ints (many dups)" mkrandDup chkrandDup
  t cmpInt "random ints (few dups)" mkrandNodup chkrandNodup
  t cmpstr "records (str)" (\e -> mkrecs e 1) (\e -> chkstr e 1)
  forM_ [1, 10, 100, 1000] $ \m ->
    t cmpint ("records (int[" ++ show m ++ "])") (\e -> mkrecs e m) (\e -> chkint e m)
  when stable $ forM_ [1, 10, 100, 1000] $ \m ->
    t cmpint ("records (int[" ++ show m ++ "]) [stable]") (\e -> mkrecs e m) (\e -> chklex e m)

-- One round: the program's output
round' :: Int -> IO String
round' k = do
  env <- Env <$> newIORef 0 <*> newIORef [] <*> newIORef 0
  printf' env "Command line arguments are:\n"
  test env k "List.sort" False (onList listSort)
  test env k "List.stable_sort" True (onList listStableSort)
  test env k "Array.sort" False (onArray arraySort)
  test env k "Array.stable_sort" True (onArray arrayStableSort)
  nf <- readIORef (numFailed env)
  printf' env ("Number of tests failed: " ++ show nf ++ "\n")
  concat . reverse <$> readIORef (out env)

main :: IO ()
main = do
  n <- iterations 6
  outs <- mapM (\i -> round' (vary i 0 0)) [1 .. n]
  putStr (if all (== head outs) outs then head outs else "fail\n")
