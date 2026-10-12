-- bdd.ml (OCaml testsuite tests/misc; Xavier Leroy's translation of an SML
-- original): binary decision diagrams with a hash-consing table and memo
-- caches in mutable arrays, building the hidden-weighted-bit function on 20
-- variables and testing it on 10 random inputs.  Prints "OK", as the
-- OCaml does.  The OCaml's global tables are a state record created afresh
-- each round (node ids restart, so caches cannot be shared between rounds).
module Main (main) where

import Control.Monad (forM_)
import Data.Array.IO
import Data.Bits (shiftL, (.&.))
import Data.IORef
import Repeat

data Bdd = One | Zero | Node Bdd Int Int Bdd

eval :: Bdd -> IOUArray Int Bool -> IO Bool
eval bdd vars = case bdd of
  Zero -> return False
  One -> return True
  Node l v _ h -> readArray vars v >>= \b -> if b then eval h vars else eval l vars

getId :: Bdd -> Int
getId (Node _ _ i _) = i
getId Zero = 0
getId One = 1

initSize_1 :: Int
initSize_1 = 8 * 1024 - 1

cacheSize :: Int
cacheSize = 1999

data St = St
  { nodeC :: IORef Int, sz_1 :: IORef Int, htab :: IORef (IOArray Int [Bdd]), nItems :: IORef Int
  , andslot1, andslot2, notslot1 :: IOUArray Int Int
  , andslot3, notslot2 :: IOArray Int Bdd }

newSt :: IO St
newSt = St <$> newIORef 1 <*> newIORef initSize_1 <*> (newArray (0, initSize_1) [] >>= newIORef) <*> newIORef 0
           <*> newArray (0, cacheSize - 1) 0 <*> newArray (0, cacheSize - 1) 0 <*> newArray (0, cacheSize - 1) 0
           <*> newArray (0, cacheSize - 1) Zero <*> newArray (0, cacheSize - 1) One

hashVal :: Int -> Int -> Int -> Int
hashVal x y v = (x `shiftL` 1) + y + (v `shiftL` 2)

resize :: St -> Int -> IO ()
resize st newSize = do
  arr <- readIORef (htab st)
  let newSz_1 = newSize - 1
  newArr <- newArray (0, newSize - 1) []
  let copyBucket [] = return ()
      copyBucket (n : ns) = case n of
        Node l v _ h -> do
          let ind = hashVal (getId l) (getId h) v .&. newSz_1
          old <- readArray newArr ind
          writeArray newArr ind (n : old)
          copyBucket ns
        _ -> error "assert false"
  s1 <- readIORef (sz_1 st)
  forM_ [0 .. s1] $ \n -> readArray arr n >>= copyBucket
  writeIORef (htab st) newArr
  writeIORef (sz_1 st) newSz_1

insert :: St -> Int -> Int -> Int -> Int -> [Bdd] -> Bdd -> IO ()
insert st idl idh v ind bucket newNode = do
  ni <- readIORef (nItems st)
  s1 <- readIORef (sz_1 st)
  if ni <= s1
    then do { t <- readIORef (htab st); writeArray t ind (newNode : bucket); writeIORef (nItems st) (ni + 1) }
    else do
      resize st (s1 + s1 + 2)
      s1' <- readIORef (sz_1 st)
      let ind' = hashVal idl idh v .&. s1'
      t <- readIORef (htab st)
      old <- readArray t ind'
      writeArray t ind' (newNode : old)

mkNode :: St -> Bdd -> Int -> Bdd -> IO Bdd
mkNode st low v high = do
  let idl = getId low
      idh = getId high
  if idl == idh then return low else do
    s1 <- readIORef (sz_1 st)
    let ind = hashVal idl idh v .&. s1
    t <- readIORef (htab st)
    bucket <- readArray t ind
    let lookup' [] = do
          modifyIORef' (nodeC st) (+ 1)
          i <- readIORef (nodeC st)
          let n = Node low v i high
          insert st (getId low) (getId high) v ind bucket n
          return n
        lookup' (n : ns) = case n of
          Node l v' _ h -> if v == v' && idl == getId l && idh == getId h then return n else lookup' ns
          _ -> error "assert false"
    lookup' bucket

mkVar :: St -> Int -> IO Bdd
mkVar st x = mkNode st Zero x One

hash :: Int -> Int -> Int
hash x y = ((x `shiftL` 1) + y) `mod` cacheSize

notB :: St -> Bdd -> IO Bdd
notB st n = case n of
  Zero -> return One
  One -> return Zero
  Node l v i r -> do
    let h = i `mod` cacheSize
    s1 <- readArray (notslot1 st) h
    if i == s1 then readArray (notslot2 st) h else do
      nl <- notB st l
      nr <- notB st r
      f <- mkNode st nl v nr
      writeArray (notslot1 st) h i
      writeArray (notslot2 st) h f
      return f

-- and2 and xor share the and cache, as in the OCaml
binOp :: (St -> Bdd -> Bdd -> IO Bdd) -> St -> Bdd -> Int -> Int -> Bdd -> Bdd -> Bdd -> Int -> Int -> Bdd -> Bdd -> IO Bdd
binOp self st n1 v1 i1 l1 r1 n2 v2 i2 l2 r2 = do
  let h = hash i1 i2
  a1 <- readArray (andslot1 st) h
  a2 <- readArray (andslot2 st) h
  if i1 == a1 && i2 == a2 then readArray (andslot3 st) h else do
    f <- case compare v1 v2 of
      EQ -> do { a <- self st l1 l2; b <- self st r1 r2; mkNode st a v1 b }
      LT -> do { a <- self st l1 n2; b <- self st r1 n2; mkNode st a v1 b }
      GT -> do { a <- self st n1 l2; b <- self st n1 r2; mkNode st a v2 b }
    writeArray (andslot1 st) h i1
    writeArray (andslot2 st) h i2
    writeArray (andslot3 st) h f
    return f

and2 :: St -> Bdd -> Bdd -> IO Bdd
and2 st n1 n2 = case n1 of
  Node l1 v1 i1 r1 -> case n2 of
    Node l2 v2 i2 r2 -> binOp and2 st n1 v1 i1 l1 r1 n2 v2 i2 l2 r2
    Zero -> return Zero
    One -> return n1
  Zero -> return Zero
  One -> return n2

xor :: St -> Bdd -> Bdd -> IO Bdd
xor st n1 n2 = case n1 of
  Node l1 v1 i1 r1 -> case n2 of
    Node l2 v2 i2 r2 -> binOp xor st n1 v1 i1 l1 r1 n2 v2 i2 l2 r2
    Zero -> return n1
    One -> notB st n1
  Zero -> return n2
  One -> notB st n2

hwb :: St -> Int -> IO Bdd
hwb st n = h 0 (n - 1)
  where
    h i j | i == j = mkVar st i
          | otherwise = do
              a <- mkVar st j >>= notB st
              b <- h i (j - 1)
              x <- and2 st a b
              c <- mkVar st j
              d <- g i (j - 1)
              y <- and2 st c d
              xor st x y
    g i j | i == j = mkVar st i
          | otherwise = do
              a <- mkVar st i >>= notB st
              b <- h (i + 1) j
              x <- and2 st a b
              c <- mkVar st i
              d <- g (i + 1) j
              y <- and2 st c d
              xor st x y

-- One round: build hwb n, test it on ntests random inputs
round' :: Int -> Int -> IO Bool
round' n ntests = do
  st <- newSt
  seed <- newIORef (0 :: Int)
  let random = do
        modifyIORef' seed (\s -> s * 25173 + 17431)
        s <- readIORef seed
        return (s .&. 1 > 0)
  bdd <- hwb st n
  results <- mapM (\_ -> do
    vars <- newArray (0, n - 1) False :: IO (IOUArray Int Bool)
    forM_ [0 .. n - 1] $ \i -> random >>= writeArray vars i
    vs <- getElems vars
    let ntrue = length (filter id vs)
    e <- eval bdd vars
    return (e == (if ntrue > 0 then vs !! (ntrue - 1) else False))) [1 .. ntests]
  return (and results)

main :: IO ()
main = do
  n <- iterations 15
  oks <- mapM (\i -> round' (vary i 20 0) 10) [1 .. n]
  putStrLn (if and oks then "OK" else "FAILED")
