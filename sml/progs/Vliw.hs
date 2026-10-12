-- vliw.sml (MLton benchmarks, 3cps test/programs/from-mlton; the SML/NJ
-- suite, John Danskin): a VLIW instruction scheduler.  It reads an abstract
-- machine program (DATA/ndotprod.s, embedded here as a string), splits
-- instructions, inserts bogus delay instructions, builds a graph of nodes
-- in a mutable hash table (the program), percolates operations up through
-- a window of 9 nodes (move_op, delete), and writes the program before and
-- after compression.  Runs in IO.  The SML writes the two programs to
-- files; this collects them per round and prints one round's, as the
-- driver in tools/mlton/vliw.sml prints the files.  The SML's debugging
-- flags (refs it sets to false, with print a no-op) and do_move_tests and
-- do_move_ops (never assigned) are constants, and the code only they reach
-- (move_test, the debugging printers, the simulator SetEnv/SimStuff, which
-- Main never calls) is not translated.  Exceptions that would end the
-- program are errors; DAGnotfound, which attach handles, is Nothing.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless)
import Data.Array.IO
import Data.Char (ord)
import Data.IORef
import Data.Maybe (fromMaybe)
import Numeric (showFFloat)
import System.IO.Unsafe (unsafePerformIO)
import Repeat

smlInt :: Int -> String
smlInt n = if n < 0 then '~' : show (negate n) else show n

-- SML's Real.toString (%.12g)
smlReal :: Double -> String
smlReal v
  | v < 0 = '~' : smlReal (negate v)
  | v == 0 = "0"
  | e < -4 || e >= 12 = let (m, ex) = sci in m ++ "E" ++ (if ex < 0 then '~' : show (negate ex) else show ex)
  | otherwise = trim (showFFloat (Just (11 - e)) v "")
  where
    e = floor (logBase 10 v) :: Int
    sci = let m = v / 10 ^^ e in (trim (showFFloat (Just 11) m ""), e)
    trim str = case break (== '.') str of
      (i, '.' : f) -> let f' = reverse (dropWhile (== '0') (reverse f)) in if null f' then i else i ++ "." ++ f'
      _ -> str

intToReal :: Int -> Double
intToReal x = read (show x ++ ".0")

-- fold f l b = List.foldr f b l, with effects (the last element first)
foldrIO :: (a -> b -> IO b) -> [a] -> b -> IO b
foldrIO f xs z = go xs
  where go [] = return z
        go (y : ys) = go ys >>= f y

------------------------------------------------------------------------
-- Stringmap: a hash table of buckets

newtype Stringmap a = SM (IOArray Int [(String, a)])

tableSize :: Int
tableSize = 2357

hash :: String -> Int
hash str = loop str 0
  where loop [] r = r
        loop (c : cs) r = loop cs ((32 * r + ord c) `mod` tableSize)

smNew :: IO (Stringmap a)
smNew = SM <$> newArray (0, tableSize - 1) []

smAdd :: Stringmap a -> (String, a) -> IO ()
smAdd (SM a) (s, x) = let index = hash s in readArray a index >>= \b -> writeArray a index ((s, x) : b)

smMap :: Stringmap a -> String -> IO (Maybe a)
smMap (SM a) s = find' <$> readArray a (hash s)
  where find' ((s', x) : r) = if s == s' then Just x else find' r
        find' [] = Nothing

smIsin :: Stringmap a -> String -> IO Bool
smIsin a s = maybe False (const True) <$> smMap a s

smRm :: Stringmap a -> String -> IO ()
smRm (SM a) s = do
  let f (b@(s', _) : r) = if s == s' then f r else b : f r
      f [] = []
      index = hash s
  readArray a index >>= writeArray a index . f

smExtract :: Stringmap a -> IO [a]
smExtract (SM a) = do
  al <- getElems a
  return (map snd (foldr (++) [] al))

stringListString :: [String] -> String
stringListString l = "[" ++ sl l
  where sl [] = "]"
        sl [h] = h ++ "]"
        sl (h : n : t) = h ++ "," ++ sl (n : t)

-- Sort: the SML's merge sort on lists of singletons
sortBy' :: (a -> a -> Bool) -> [a] -> [a]
sortBy' gt l = ml (map (: []) l)
  where
    m2 [] b = b
    m2 a [] = a
    m2 (ha : ta) (hb : tb) = if gt ha hb then hb : m2 (ha : ta) tb else ha : m2 ta (hb : tb)
    ml [] = []
    ml [h] = h
    ml [h1, h2] = m2 h1 h2
    ml (h1 : h2 : rest) = ml [m2 h1 h2, ml rest]

------------------------------------------------------------------------
-- Set: lists with an equality

data Set a = S (a -> a -> Bool) [a]

makeSet :: Eq a => Set a
makeSet = S (==) []

makeEQ :: (a -> a -> Bool) -> Set a
makeEQ eqf = S eqf []

set :: Set a -> [a]
set (S _ a) = a

member :: Set a -> a -> Bool
member (S _ []) _ = False
member (S eqf (s : t)) e = eqf e s || member (S eqf t) e

add :: Set a -> a -> Set a
add st@(S eqf s) e = if member st e then st else S eqf (e : s)

listToSetEQ :: (a -> a -> Bool) -> [a] -> Set a
listToSetEQ eqf l = f l (makeEQ eqf)
  where f [] s = s
        f (h : t) s = f t (add s h)

listToSet :: Eq a => [a] -> Set a
listToSet = listToSetEQ (==)

union :: Set a -> Set a -> Set a
union a (S _ []) = a
union (S _ []) b = b
union (S eqf (e : a)) b = union (S eqf a) (add b e)

listUnion :: [Set a] -> Set a
listUnion (h : t) = foldr union h t
listUnion _ = error "LISTUNION"

listUnionEQ :: (a -> a -> Bool) -> [Set a] -> Set a
listUnionEQ eqf l = foldr union (makeEQ eqf) l

rm :: Set a -> a -> Set a
rm (S _ []) _ = error "SET"
rm (S eqf (s : t)) x = if eqf s x then S eqf t else S eqf (s : set (rm (S eqf t) x))

intersect :: Set a -> Set a -> Set a
intersect a0 b0 = intersect1 a0 b0 []
  where intersect1 _ (S eqf []) c = S eqf c
        intersect1 (S eqf []) _ c = S eqf c
        intersect1 (S eqf (a : t)) b c =
          if member b a then intersect1 (S eqf t) b (a : c) else intersect1 (S eqf t) b c

diff :: Set a -> Set a -> Set a
diff (S eqf []) _ = S eqf []
diff (S eqf (a : t)) b = if member b a then diff (S eqf t) b else S eqf (a : set (diff (S eqf t) b))

emptySet :: Set a -> Bool
emptySet (S _ []) = True
emptySet (S _ _) = False

------------------------------------------------------------------------
-- AbsMach

type Reg = (Int, String)
type Label = (Int, String)

data ArithOp = Imul | Iadd | Isub | Idiv | Orb | Andb | Xorb | Rshift | Lshift
             | Fadd | Fdiv | Fmul | Fsub | Real | Floor | Logb
  deriving Eq

data Comparison = Ilt | Ieq | Igt | Ile | Ige | Ine | Flt | Feq | Fgt | Fle | Fge | Fne
                | Inrange | Outofrange
  deriving Eq

data Opcode
  = FETCH Bool Int Reg Reg            -- immutable, offset, ptr, dst
  | STORE Int Reg Reg                 -- offset, src, ptr
  | GETLAB Label Reg                  -- lab, dst
  | GETREAL String Reg                -- value, dst
  | ARITH ArithOp Reg Reg Reg         -- oper, src1, src2, dst
  | ARITHI ArithOp Reg Int Reg        -- oper, src1, src2, dst
  | MOVE Reg Reg                      -- src, dst
  | BRANCH Comparison Reg Reg Label [Reg]  -- test, src1, src2, dst, live
  | JUMP Reg [Reg]                    -- dst, live
  | LABEL Label [Reg]                 -- lab, live
  | WORD Int
  | LABWORD Label
  | NOP
  | BOGUS [Reg] [Reg]                 -- reads, writes
  deriving Eq

-- AbsMachImp (HM)

immutableMem, mutableMem, flowControl :: Int
immutableMem = -1
mutableMem = -2
flowControl = -3

srl :: [Reg] -> Set Int
srl rl = listToSet (map fst rl)

sr :: Reg -> Set Int
sr r = srl [r]

resourcesOk :: [Opcode] -> [Opcode] -> Bool
resourcesOk ops c = length ops <= 4 && length c <= 1

allocptr :: Reg -> Bool
allocptr r = fst r == 1

writeO :: Opcode -> Set Int
writeO i = case i of
  FETCH _ _ _ dst -> sr dst
  STORE _ _ ptr -> if allocptr ptr then listToSet [immutableMem, mutableMem] else listToSet [mutableMem]
  GETLAB _ dst -> sr dst
  GETREAL _ dst -> sr dst
  ARITH _ _ _ dst -> sr dst
  ARITHI _ _ _ dst -> sr dst
  MOVE _ dst -> sr dst
  JUMP _ _ -> listToSet [flowControl]
  BOGUS _ writes -> srl writes
  _ -> makeSet

writeC :: Opcode -> Set Int
writeC _ = listToSet [flowControl]

stdRegList :: [Reg]
stdRegList = [(1, ""), (2, ""), (3, ""), (4, ""), (5, "")]

readOp :: Opcode -> Set Int
readOp i = case i of
  FETCH immutable _ ptr _ -> add (sr ptr) (if immutable then immutableMem else mutableMem)
  STORE _ src ptr -> srl [src, ptr]
  ARITH _ src1 src2 _ -> srl [src1, src2]
  ARITHI _ src1 _ _ -> sr src1
  MOVE src _ -> sr src
  BRANCH _ src1 src2 _ _ -> srl [src1, src2]
  JUMP dst _ -> srl (dst : stdRegList)
  BOGUS reads _ -> srl reads
  _ -> makeSet

data CodeTypes = ASSIGNMENT Opcode | LABELREF Int Opcode | COMPARISON Int Opcode | FLOW Int Opcode
               | TARGET Int Opcode | EXIT Opcode | JUNK Opcode | NERGLE

maxreg :: [Opcode] -> Int
maxreg li = foldr max 0 (set (listUnion (map writeO li ++ map readOp li)))

classify :: Opcode -> CodeTypes
classify i = case i of
  FETCH {} -> ASSIGNMENT i
  STORE {} -> ASSIGNMENT i
  GETLAB lab _ -> LABELREF (fst lab) i
  GETREAL {} -> ASSIGNMENT i
  ARITH {} -> ASSIGNMENT i
  ARITHI {} -> ASSIGNMENT i
  MOVE src dst -> if fst src == fst dst then NERGLE else ASSIGNMENT i
  BRANCH test src1 src2 dst _ ->
    if test == Ieq && fst src1 == fst src2 then FLOW (fst dst) i else COMPARISON (fst dst) i
  JUMP {} -> EXIT i
  LABEL lab _ -> TARGET (fst lab) i
  WORD _ -> JUNK i
  LABWORD _ -> JUNK i
  NOP -> JUNK i
  BOGUS {} -> ASSIGNMENT i

------------------------------------------------------------------------
-- ReadAbs

readAbs :: [String] -> [Opcode]
readAbs = readline (0 :: Int)
  where
    readline _ [] = []
    readline i (ln : lns) = case ln ++ "\n" of
      [] -> []
      l -> opcode (b l) : readline (i + 1) lns
      where
        err s = error ("Error in line " ++ smlInt i ++ ": " ++ s)
        b (' ' : rest) = b rest
        b rest = rest
        aop l = case l of
          'i' : 'm' : 'u' : 'l' : r -> (Imul, r)
          'i' : 'a' : 'd' : 'd' : r -> (Iadd, r)
          'i' : 's' : 'u' : 'b' : r -> (Isub, r)
          'i' : 'd' : 'i' : 'v' : r -> (Idiv, r)
          'o' : 'r' : 'b' : ' ' : r -> (Orb, r)
          'a' : 'n' : 'd' : 'b' : r -> (Andb, r)
          'x' : 'o' : 'r' : 'b' : r -> (Xorb, r)
          'r' : 's' : 'h' : 'i' : 'f' : 't' : r -> (Rshift, r)
          'l' : 's' : 'h' : 'i' : 'f' : 't' : r -> (Lshift, r)
          'f' : 'a' : 'd' : 'd' : r -> (Fadd, r)
          'f' : 'd' : 'i' : 'v' : r -> (Fdiv, r)
          'f' : 'm' : 'u' : 'l' : r -> (Fmul, r)
          'f' : 's' : 'u' : 'b' : r -> (Fsub, r)
          'r' : 'e' : 'a' : 'l' : r -> (Real, r)
          'f' : 'l' : 'o' : 'o' : 'r' : r -> (Floor, r)
          'l' : 'o' : 'g' : 'b' : r -> (Logb, r)
          _ -> err "illegal arithmetic operator"
        com l = case l of
          'i' : 'l' : 't' : r -> (Ilt, r)
          'i' : 'e' : 'q' : r -> (Ieq, r)
          'i' : 'g' : 't' : r -> (Igt, r)
          'i' : 'l' : 'e' : r -> (Ile, r)
          'i' : 'g' : 'e' : r -> (Ige, r)
          'i' : 'n' : 'e' : r -> (Ine, r)
          'f' : 'l' : 't' : r -> (Flt, r)
          'f' : 'e' : 'q' : r -> (Feq, r)
          'f' : 'g' : 't' : r -> (Fgt, r)
          'f' : 'l' : 'e' : r -> (Fle, r)
          'f' : 'g' : 'e' : r -> (Fge, r)
          'f' : 'n' : 'e' : r -> (Fne, r)
          'i' : 'n' : 'r' : 'a' : 'n' : 'g' : 'e' : r -> (Inrange, r)
          'o' : 'u' : 't' : 'o' : 'f' : 'r' : 'a' : 'n' : 'g' : 'e' : r -> (Outofrange, r)
          _ -> err "illegal comparison operator"
        immut ('i' : l) = (True, l)
        immut ('m' : l) = (False, l)
        immut _ = err "i or m required"
        int l = f 0 l
          where f n l0@(d : l') = if d >= '0' && d <= '9' then f (n * 10 + ord d - ord '0') l' else (n, l0)
                f _ _ = err "in readabs.int"
        string l = f l
          where f ('/' : l') = ([], l')
                f (a : l') = let (s, l'') = f l' in (a : s, l'')
                f _ = err "name not terminated by \"/\""
        realc s0 =
          let (sign, s) = case s0 of { '~' : rest -> (-1.0, rest); _ -> (1.0, s0) }
              j ex (d : dl) mant = j ex dl (mant * 0.1 + intToReal d)
              j 0 [] mant = mant * sign
              j ex [] mant = if ex > 0 then j (ex - 1) [] (mant * 10.0) else j (ex + 1) [] (mant * 0.1)
              h esign wholedigits diglist ex [] = j (esign * ex + wholedigits - 1) diglist 0.0
              h es fd dl ex (d : s') = h es fd dl (ex * 10 + (ord d - ord '0')) s'
              g i' r ('E' : '~' : s') = h (-1) i' r 0 s'
              g i' r ('E' : s') = h 1 i' r 0 s'
              g i' r (d : s') = if d >= '0' && d <= '9' then g i' ((ord d - ord '0') : r) s' else h 1 i' r 0 []
              g i' r [] = h 1 i' r 0 []
              f i' r ('.' : s') = g i' r s'
              f i' r s'@('E' : _) = g i' r s'
              f i' r (d : s') = f (i' + 1) ((ord d - ord '0') : r) s'
              f _ _ _ = err "bad in readdabs"
          in f (0 :: Int) [] s :: Double
        require (a : ar) (b' : br) = if a == b' then require ar br else err (a : " required")
        require [] br = br
        require (a : _) _ = err (a : " required")
        reg l = let (s, l1) = string l
                    l2 = require "R" l1
                    (i', l3) = int l2
                in ((i', s), l3)
        lab l = let (s, l1) = string l
                    l2 = require "L" l1
                    (i', l3) = int l2
                in ((i', s), l3)
        live l = f (b (require "(" l))
          where f (')' : _) = []
                f l' = let (r, l'') = reg l' in r : f (b l'')
        opcode l = case l of
          'F' : 'E' : 'T' : 'C' : 'H' : l0 ->
            let (imm, l1) = immut (b l0)
                (dst, l2) = reg (b l1)
                (ptr, l3) = reg (b (require "M[" (b (require ":=" (b l2)))))
                (offset, l4) = int (b (require "+" (b l3)))
            in require "]" (b l4) `seq` FETCH imm offset ptr dst
          'S' : 'T' : 'O' : 'R' : 'E' : l0 ->
            let (ptr, l1) = reg (b (require "M[" (b l0)))
                (offset, l2) = int (b (require "+" (b l1)))
                (src, _) = reg (b (require ":=" (b (require "]" (b l2)))))
            in STORE offset src ptr
          'G' : 'E' : 'T' : 'L' : 'A' : 'B' : l0 ->
            let (dst, l1) = reg (b l0)
                (lb, _) = lab (b (require ":=" (b l1)))
            in GETLAB lb dst
          'G' : 'E' : 'T' : 'R' : 'E' : 'A' : 'L' : l0 ->
            let (dst, l1) = reg (b l0)
                r = realc (b (require ":=" (b l1)))
            in GETREAL (smlReal r) dst
          'A' : 'R' : 'I' : 'T' : 'H' : 'I' : l0 ->
            let (dst, l1) = reg (b l0)
                (s1, l2) = reg (b (require ":=" (b l1)))
                (oper, l3) = aop (b l2)
                (s2, _) = int (b l3)
            in ARITHI oper s1 s2 dst
          'A' : 'R' : 'I' : 'T' : 'H' : l0 ->
            let (dst, l1) = reg (b l0)
                (s1, l2) = reg (b (require ":=" (b l1)))
                (oper, l3) = aop (b l2)
                (s2, _) = reg (b l3)
            in ARITH oper s1 s2 dst
          'M' : 'O' : 'V' : 'E' : l0 ->
            let (dst, l1) = reg (b l0)
                (s1, _) = reg (b (require ":=" (b l1)))
            in MOVE s1 dst
          'B' : 'R' : 'A' : 'N' : 'C' : 'H' : l0 ->
            let (s1, l1) = reg (b (require "IF" (b l0)))
                (test, l2) = com (b l1)
                (s2, l3) = reg (b l2)
                (dst, l4) = lab (b (require "GOTO" (b l3)))
                liv = live (b l4)
            in BRANCH test s1 s2 dst liv
          'J' : 'U' : 'M' : 'P' : l0 ->
            let (dst, l1) = reg (b l0)
                liv = live (b l1)
            in JUMP dst liv
          'L' : 'A' : 'B' : 'E' : 'L' : l0 ->
            let (lb, l1) = lab (b l0)
                liv = live (b (require ":" l1))
            in LABEL lb liv
          'W' : 'O' : 'R' : 'D' : l0 -> WORD (fst (int (b l0)))
          'L' : 'A' : 'B' : 'W' : 'O' : 'R' : 'D' : l0 -> LABWORD (fst (lab (b l0)))
          'N' : 'O' : 'P' : _ -> NOP
          _ -> err "illegal opcode name"

------------------------------------------------------------------------
-- PrintAbs

xstr :: Opcode -> String
xstr i = foldl (++) "" (p i)
  where
    aop o = case o of
      Imul -> "imul"; Iadd -> "iadd"; Isub -> "isub"; Idiv -> "idiv"; Orb -> "orb"
      Andb -> "andb"; Xorb -> "xorb"; Rshift -> "rshift"; Lshift -> "lshift"; Fadd -> "fadd"
      Fdiv -> "fdiv"; Fmul -> "fmul"; Fsub -> "fsub"; Real -> "real"; Floor -> "floor"; Logb -> "logb"
    com c = case c of
      Ilt -> "ilt"; Ieq -> "ieq"; Igt -> "igt"; Ile -> "ile"; Ige -> "ige"; Ine -> "ine"
      Flt -> "flt"; Feq -> "feq"; Fgt -> "fgt"; Fle -> "fle"; Fge -> "fge"; Fne -> "fne"
      Inrange -> "inrange"; Outofrange -> "outofrange"
    reg (n, s) = [s, "/R", smlInt n]
    label (n, s) = [s, "/L", smlInt n]
    regs = concatMap (\r -> reg r ++ [" "])
    p op = case op of
      FETCH immutable offset ptr dst ->
        ["FETCH", if immutable then "i  " else "m  "] ++ reg dst ++ [" := M[ "] ++ reg ptr
          ++ [" + ", smlInt offset, " ]\n"]
      STORE offset src ptr ->
        ["STORE   ", "M[ "] ++ reg ptr ++ [" + ", smlInt offset, " ] := "] ++ reg src ++ ["\n"]
      GETLAB lab dst -> ["GETLAB  "] ++ reg dst ++ [" := "] ++ label lab ++ ["\n"]
      GETREAL value dst -> ["GETREAL "] ++ reg dst ++ [" := ", value, "\n"]
      ARITH oper src1 src2 dst ->
        ["ARITH   "] ++ reg dst ++ [" := "] ++ reg src1 ++ [" ", aop oper, " "] ++ reg src2 ++ ["\n"]
      ARITHI oper src1 src2 dst ->
        ["ARITHI  "] ++ reg dst ++ [" := "] ++ reg src1 ++ [" ", aop oper, " ", smlInt src2, "\n"]
      MOVE src dst -> ["MOVE    "] ++ reg dst ++ [" := "] ++ reg src ++ ["\n"]
      BRANCH test src1 src2 dst live ->
        ["BRANCH  ", "IF "] ++ reg src1 ++ [" ", com test, " "] ++ reg src2 ++ [" GOTO "] ++ label dst
          ++ ["   ( "] ++ regs live ++ [")\n"]
      JUMP dst live -> ["JUMP    "] ++ reg dst ++ ["   ( "] ++ regs live ++ [")\n"]
      LABEL lab live -> ["LABEL   "] ++ label lab ++ [":      ( "] ++ regs live ++ [")\n"]
      WORD value -> ["WORD    ", smlInt value, "\n"]
      LABWORD lab -> ["LABWORD "] ++ label lab ++ ["\n"]
      NOP -> ["NOP\n"]
      BOGUS reads writes -> ["BOGUS", "   ( "] ++ regs writes ++ [") := ("] ++ regs reads ++ [")\n"]

-- PrintAbs.show, into a buffer in place of a file
showProg :: IORef [String] -> [Opcode] -> IO ()
showProg out prog = forM_ prog $ \h -> modifyIORef out (xstr h :)

------------------------------------------------------------------------
-- BreakInst

newRegVal :: IORef Int
newRegVal = unsafePerformIO (newIORef 0)
{-# NOINLINE newRegVal #-}

newRegPairs :: IORef [(Reg, Reg)]
newRegPairs = unsafePerformIO (newIORef [])
{-# NOINLINE newRegPairs #-}

newRegInit :: [Opcode] -> IO ()
newRegInit li = writeIORef newRegVal (maxreg li) >> writeIORef newRegPairs []

newReg :: Reg -> IO Reg
newReg r = readIORef newRegPairs >>= f
  where f [] = do
          modifyIORef' newRegVal (+ 1)
          v <- readIORef newRegVal
          let nr = (v, snd r)
          modifyIORef newRegPairs ((r, nr) :)
          return nr
        f ((a, b) : t) = if r == a then return b else f t

breaki :: [Opcode] -> IO [Opcode]
breaki l = do
  let f i = case i of
        ARITH oper src1 src2 dst ->
          if fst dst == fst src1 || fst dst == fst src2
            then newReg dst >>= \nr -> return [ARITH oper src2 src2 nr, MOVE nr dst]
            else return [i]
        ARITHI oper src1 src2 dst ->
          if fst dst == fst src1
            then newReg dst >>= \nr -> return [ARITHI oper src1 src2 nr, MOVE nr dst]
            else return [i]
        FETCH immutable offset ptr dst ->
          if fst ptr == fst dst
            then newReg dst >>= \nr -> return [FETCH immutable offset ptr nr, MOVE nr dst]
            else return [i]
        MOVE src dst -> return (if fst src == fst dst then [] else [i])
        _ -> return [i]
      h a b = (++ b) <$> f a
  newRegInit l
  foldrIO h l []

-- OutFilter

remnops :: [Opcode] -> [Opcode]
remnops ol = foldr f [] ol
  where f NOP (NOP : b) = NOP : b
        f a b = a : b

------------------------------------------------------------------------
-- Delay

maxregRef, maxBogReg, currIdemReg, idempotency :: IORef Int
maxregRef = unsafePerformIO (newIORef 0)
{-# NOINLINE maxregRef #-}
maxBogReg = unsafePerformIO (newIORef 0)
{-# NOINLINE maxBogReg #-}
currIdemReg = unsafePerformIO (newIORef 0)
{-# NOINLINE currIdemReg #-}
idempotency = unsafePerformIO (newIORef 0)
{-# NOINLINE idempotency #-}

maxdelay :: Int
maxdelay = 12

isBogusI :: Opcode -> Bool
isBogusI (BOGUS _ _) = True
isBogusI _ = False

bogusReg :: Int -> Reg -> Int -> Reg
bogusReg mr (i, s) which = (mr + maxdelay * i + which, s)

isBogusReg :: Int -> Reg -> Bool
isBogusReg mr (i, _) = i > mr

unbogusReg :: Int -> Reg -> Reg
unbogusReg mr (i, s) = if isBogusReg mr (i, s) then (i `div` maxdelay, s) else (i, s)

idemReg :: IO Reg
idemReg = do
  modifyIORef' currIdemReg (+ 1)
  r <- readIORef currIdemReg
  return (r, "idem")

delayInit :: [Opcode] -> IO ()
delayInit il = do
  writeIORef maxregRef (maxreg il)
  mr <- readIORef maxregRef
  writeIORef maxBogReg ((mr + 1) * maxdelay)
  readIORef maxBogReg >>= writeIORef currIdemReg . (+ 1)

delay :: Opcode -> Int
delay i =
  let opdelay o = case o of
        Imul -> 5; Iadd -> 2; Isub -> 2; Idiv -> 12; Orb -> 2; Andb -> 2; Xorb -> 2
        Rshift -> 2; Lshift -> 2; Fadd -> 2; Fdiv -> 12; Fmul -> 4; Fsub -> 2; Real -> 2
        Floor -> 2; Logb -> 2
  in case i of
       FETCH {} -> 2
       STORE {} -> 2
       GETLAB {} -> 2
       GETREAL {} -> 2
       ARITH oper _ _ _ -> opdelay oper
       ARITHI oper _ _ _ -> opdelay oper
       MOVE {} -> 1
       BRANCH {} -> 5
       JUMP {} -> 1
       LABEL {} -> 0
       NOP -> 1
       _ -> error "DELAY"

bIdemx :: Int -> [Reg] -> [Reg] -> IO [Opcode]
bIdemx 0 _ _ = return []
bIdemx 1 r w = idemReg >>= \ir -> return [BOGUS (r ++ w) [ir]]
bIdemx n r w = do
  ir <- idemReg
  rest <- bIdemx (n - 1) r [ir]
  return (BOGUS (r ++ w) [ir] : rest)

bIdem :: Int -> [Reg] -> [Reg] -> IO [Opcode]
bIdem n r w =
  let fil (i, s) b = if i == 0 then b else (i, s) : b
      nr = foldr fil [] r
  in if null nr then return [] else bIdemx n nr w

bAssx :: Int -> Int -> Reg -> [Opcode]
bAssx _ 0 _ = []
bAssx mr 1 r = [BOGUS [bogusReg mr r 1] [r]]
bAssx mr n r = BOGUS [bogusReg mr r n] [bogusReg mr r (n - 1)] : bAssx mr (n - 1) r

bAss :: Int -> Int -> Reg -> [Opcode]
bAss mr n r = BOGUS [r] [bogusReg mr r (n - 1)] : bAssx mr (n - 1) r

bBrxx :: Int -> Int -> [Reg] -> [Opcode]
bBrxx _ 0 _ = []
bBrxx mr 1 rl = [BOGUS rl (map (\r -> bogusReg mr r 1) rl)]
bBrxx mr n rl = BOGUS (map (\r -> bogusReg mr r (n - 1)) rl) (map (\r -> bogusReg mr r n) rl) : bBrxx mr (n - 1) rl

bBrx :: Int -> Int -> [Reg] -> [Opcode]
bBrx mr n rl = BOGUS (map (\r -> bogusReg mr r (n - 1)) rl) rl : bBrxx mr (n - 1) rl

bBr :: Int -> Opcode -> Int -> [Reg] -> [Opcode]
bBr mr b n rl = reverse (b : bBrx mr n rl)

isFlow :: Opcode -> Bool
isFlow i = case classify i of { FLOW _ _ -> True; _ -> False }

addDelay :: [Opcode] -> IO [Opcode]
addDelay il = do
  idemp <- readIORef idempotency
  mr <- readIORef maxregRef
  let idem r w = bIdem idemp r w
      g i = do
        let d = delay i
        case i of
          FETCH _ _ ptr dst -> idem [ptr] [dst] >>= \x -> return (i : (x ++ bAss mr d dst))
          STORE {} -> return [i]
          GETLAB _ dst -> return (i : bAss mr d dst)
          GETREAL _ dst -> return (i : bAss mr d dst)
          ARITH _ src1 src2 dst -> idem [src1, src2] [dst] >>= \x -> return (i : (x ++ bAss mr d dst))
          ARITHI _ src1 _ dst -> idem [src1] [dst] >>= \x -> return (i : (x ++ bAss mr d dst))
          MOVE src dst -> (i :) <$> idem [src] [dst]
          BRANCH test src1 src2 dst live ->
            return (if isFlow i then [i] else bBr mr (BRANCH test src1 src2 dst live) d [src1, src2])
          _ -> d `seq` return [i]
      apnd [] b = b
      apnd (a : t) b = a : apnd t b
      fld a b = (`apnd` b) <$> g a
  foldrIO fld il []

rmBogus :: [Opcode] -> IO [Opcode]
rmBogus il = do
  mr <- readIORef maxregRef
  let ub = unbogusReg mr
      g [] = []
      g (i : t) = case i of
        FETCH immutable offset ptr dst -> FETCH immutable offset ptr (ub dst) : g t
        STORE {} -> i : g t
        GETLAB lab dst -> GETLAB lab (ub dst) : g t
        GETREAL value dst -> GETREAL value (ub dst) : g t
        ARITH oper src1 src2 dst -> ARITH oper src1 src2 (ub dst) : g t
        ARITHI oper src1 src2 dst -> ARITHI oper src1 src2 (ub dst) : g t
        MOVE {} -> i : g t
        BRANCH test src1 src2 dst live -> BRANCH test (ub src1) (ub src2) dst live : g t
        BOGUS {} -> g t
        _ -> i : g t
  return (g il)

------------------------------------------------------------------------
-- Ntypes

type Name = String
type Test = Opcode
type Assignment = Opcode

teq, aeq :: Opcode -> Opcode -> Bool
teq = (==)
aeq = (==)

ct :: IORef Int
ct = unsafePerformIO (newIORef 0)
{-# NOINLINE ct #-}

initNames :: IO ()
initNames = writeIORef ct 0

nn :: IO Int
nn = modifyIORef' ct (+ 1) >> subtract 1 <$> readIORef ct

namePrefix :: Name -> Name
namePrefix = pref
  where pref [] = []
        pref ('_' : _) = []
        pref (h : t) = h : pref t

namePrefixEq :: Name -> Name -> Bool
namePrefixEq a b = namePrefix a == namePrefix b

newName :: Name -> IO Name
newName n = nn >>= \k -> return (namePrefix n ++ "_" ++ smlInt k)

primeName :: Name -> IO Name
primeName n = (++ "'") <$> newName n

data TestOrName = TEST Test | NAME Name | NEITHER deriving Eq

toneq :: TestOrName -> TestOrName -> Bool
toneq (TEST a) (TEST b) = teq a b
toneq (NAME a) (NAME b) = a == b
toneq _ _ = False

------------------------------------------------------------------------
-- Dag: the selector is Nothing where the SML raises DAGnotfound

type Sel = (Test, Bool) -> Maybe TestOrName

data Dag = D (Set Test) Sel TestOrName (Set Name)

dagMake :: Dag
dagMake = D (makeEQ teq) (const Nothing) NEITHER makeSet

testsOf :: Dag -> Set Test
testsOf (D b _ _ _) = b

selOf :: Dag -> Sel
selOf (D _ s _ _) = s

rootOf :: Dag -> TestOrName
rootOf (D _ _ r _) = r

succOf :: Dag -> Set Name
succOf (D _ _ _ h) = h

attach :: Test -> Dag -> Dag -> Dag
attach t (D b1 sel1 r1 h1) (D b2 sel2 r2 h2) =
  D (add (union b1 b2) t)
    (\(x, y) -> if teq x t then Just (if y then r1 else r2) else sel1 (x, y) <|> sel2 (x, y))
    (TEST t)
    (union h1 h2)

reach :: Dag -> TestOrName -> Dag
reach d@(D b sel _ _) tn = case tn of
  TEST t ->
    if not (member b t) then error "DAGnotfound"
    else attach t (reach d (found (sel (t, True)))) (reach d (found (sel (t, False))))
  NAME n -> D (makeEQ teq) (const Nothing) (NAME n) (listToSet [n])
  _ -> error "DAGnotfound"
  where found = fromMaybe (error "DAGnotfound")

replaceEdge :: Dag -> [Name] -> Dag
replaceEdge d [] = d
replaceEdge (D b sel r h) (old : new : _) =
  let nh = if member h old then add (rm h old) new else h
      nr = if toneq r (NAME old) then NAME new else r
      nsel xy = (\v -> if toneq v (NAME old) then NAME new else v) <$> sel xy
  in D b nsel nr nh
replaceEdge _ _ = error "DAG"

------------------------------------------------------------------------
-- Node

data Node = N Name (Set Assignment) Dag (Set Name)

type Program = (Stringmap Node, Node, Node)

nameOf :: Node -> Name
nameOf (N n _ _ _) = n

assignmentOf :: Node -> Set Assignment
assignmentOf (N _ a _ _) = a

dagOf :: Node -> Dag
dagOf (N _ _ d _) = d

predOf :: Node -> Set Name
predOf (N _ _ _ p) = p

eqn :: Node -> Node -> Bool
eqn n1 n2 = nameOf n1 == nameOf n2

n00, fin :: Node
n00 = N "START" (makeEQ aeq) dagMake makeSet
fin = N "FINISH" (makeEQ aeq) dagMake makeSet

makeProg :: IO Program
makeProg = smNew >>= \ns -> return (ns, n00, fin)

addPredNode :: Node -> Name -> Node
addPredNode (N n a t prd) p = N n a t (add prd p)

unionPredNode :: Node -> Set Name -> Node
unionPredNode (N n a t prd) ps = N n a t (union prd ps)

setPredNode :: Node -> Set Name -> Node
setPredNode (N n a t _) p = N n a t p

rmPredNode :: Node -> Name -> Node
rmPredNode (N n a t prd) p = N n a t (rm prd p)

updateNode :: Program -> Node -> IO Program
updateNode p@(ns, n0, f) newNode = do
  smRm ns (nameOf newNode)
  smAdd ns (nameOf newNode, newNode)
  return (if nameOf newNode == nameOf n0 then (ns, newNode, f)
          else if nameOf newNode == nameOf f then (ns, n0, newNode)
          else p)

addNode :: Program -> Node -> IO Program
addNode p@(ns, _, _) newNode = do
  isin <- smIsin ns (nameOf newNode)
  if isin then updateNode p newNode else smAdd ns (nameOf newNode, newNode) >> return p

rmNode :: Program -> Node -> IO Program
rmNode p@(ns, _, _) node = smRm ns (nameOf node) >> return p

succN :: Program -> Node -> Set Name
succN _ n = succOf (dagOf n)

predN :: Program -> Node -> Set Name
predN _ n = predOf n

nameToNode :: Program -> Name -> IO Node
nameToNode (ns, _, _) nm = fromMaybe (error ("NAMETONODE " ++ nm)) <$> smMap ns nm

nameSetToNodeSet :: Program -> Set Name -> IO (Set Node)
nameSetToNodeSet p ns = listToSetEQ eqn <$> mapM (nameToNode p) (set ns)

succNodes :: Program -> Node -> IO (Set Node)
succNodes p n = nameSetToNodeSet p (succN p n)

emptyNode :: Node -> Bool
emptyNode n = emptySet (assignmentOf n) && emptySet (testsOf (dagOf n))

unreachable :: Program -> Node -> Bool
unreachable p@(_, n0, _) n = not (eqn n0 n) && emptySet (predN p n)

data TestOrAssign = TST Test | ASS Assignment

readTA, writeTA :: TestOrAssign -> Set Int
readTA (TST t) = readOp t
readTA (ASS a) = readOp a
writeTA (TST t) = writeC t
writeTA (ASS a) = writeO a

readNode, writeNode :: Node -> Set Int
readNode n =
  union (listUnion (makeSet : map (readTA . ASS) (set (assignmentOf n))))
        (listUnion (makeSet : map (readTA . TST) (set (testsOf (dagOf n)))))
writeNode n =
  union (listUnion (makeSet : map (writeTA . ASS) (set (assignmentOf n))))
        (listUnion (makeSet : map (writeTA . TST) (set (testsOf (dagOf n)))))

noWriteConflict :: TestOrAssign -> Node -> Bool
noWriteConflict ta n = emptySet (intersect (writeNode n) (union (readTA ta) (writeTA ta)))

noReadConflict :: TestOrAssign -> Node -> Bool
noReadConflict ta n = emptySet (intersect (writeTA ta) (readNode n))

replaceEdgeNode :: Node -> [Name] -> Node
replaceEdgeNode (N n a d p) nl = N n a (replaceEdge d nl) p

exceptBogus :: [Opcode] -> [Opcode]
exceptBogus [] = []
exceptBogus (h : t) = if isBogusI h then exceptBogus t else h : exceptBogus t

numOpsNode, numTestsNode, numThingsNode :: Node -> Int
numOpsNode = length . exceptBogus . set . assignmentOf
numTestsNode = length . set . testsOf . dagOf
numThingsNode n = numOpsNode n + numTestsNode n

dead :: Program -> Int -> Node -> Set Name -> IO Bool
dead p r n done = do
  let newDone = add done (nameOf n)
      nfil a b = if member newDone a then b else a : b
      drl [] = return True
      drl (h : t) = dead p r h newDone >>= \x -> if x then drl t else return False
      next = foldr nfil [] (set (succN p n))
  if member (readNode n) r then return False
  else if member (writeNode n) r then return True
  else mapM (nameToNode p) next >>= drl

deadset :: Program -> Set Int -> Node -> IO Bool
deadset p rs n = f (set rs)
  where f [] = return True
        f (r : t) = dead p r n makeSet >>= \x -> if x then f t else return False

delete :: Program -> Node -> IO Program
delete p@(_, _, fn) n = do
  let em = emptyNode n
      un = unreachable p n
      ntn = nameToNode p
  pr <- listToSetEQ eqn <$> mapM ntn (set (predN p n))
  if (em || un) && not (eqn n fn)
    then
      if not un
        then do
          let s0 = set (succN p n)
              nprime = if length s0 == 1 then head s0 else error ("DELETE_WIERDSUCC " ++ smlInt (length s0))
          np <- ntn nprime
          let newNprime = rmPredNode (unionPredNode np (predOf n)) (nameOf n)
              ren x = replaceEdgeNode x [nameOf n, nameOf newNprime]
              pprime = map ren (set pr)
              updt nd prog = updateNode prog nd
          nPrime <- foldrIO updt (newNprime : pprime) p
          rmNode nPrime n
        else do
          s <- set <$> nameSetToNodeSet p (succN p n)
          let updt sn prog = updateNode prog (rmPredNode sn (nameOf n))
          foldrIO updt s p >>= \prog -> rmNode prog n
    else return p

canMoveOp1 :: Program -> Assignment -> Set Node -> Node -> IO Bool
canMoveOp1 p@(_, _, fn) x moveSet m = do
  let rok = resourcesOk (set (add (assignmentOf m) x)) (set (testsOf (dagOf m)))
  pset <- (`diff` moveSet) <$> nameSetToNodeSet p (succN p m)
  let l = writeTA (ASS x)
      dlpf [] = return True
      dlpf (pj : t) = deadset p l pj >>= \d -> if d then dlpf t else return False
      cond [] = True
      cond (nj : t) = not (eqn nj fn) && noReadConflict (ASS x) m && noWriteConflict (ASS x) m && cond t
  if rok && cond (set moveSet) then dlpf (set pset) else return False

canMoveOp :: Program -> Assignment -> Set Node -> Node -> IO Bool
canMoveOp p x moveSet m = do
  let ms = set moveSet
      ps = set (listUnion (map (predN p) ms))
      allf x' b = if b then canMoveOp1 p x' moveSet m else return False
  if length ps > 1
    then if length ms > 1 then return False
         else foldrIO allf (set (assignmentOf (head ms))) True
    else canMoveOp1 p x moveSet m

moveOp :: Program -> Assignment -> Set Node -> Node -> IO Program
moveOp p x moveSet m = do
  ok <- canMoveOp p x moveSet m
  if not ok then return p else do
    primedPairs <- newIORef []
    let pnf nm = readIORef primedPairs >>= f
          where f [] = do
                  nn' <- primeName nm
                  modifyIORef primedPairs ((nm, nn') :)
                  return nn'
                f ((a, b) : t) = if nm == a then return b else f t
        njp [] = return []
        njp (N n a d _ : t) = do
          n' <- pnf n
          rest <- njp t
          return (N n' (rm a x) d (listToSet [nameOf m]) : rest)
        ojp l = map (\y -> rmPredNode y (nameOf m)) l
        replist [] = return []
        replist (h : t) = do
          h' <- pnf h
          rest <- replist t
          return (h : h' : rest)
    rlist <- replist (map nameOf (set moveSet))
    let mprime = N (nameOf m) (add (assignmentOf m) x) (replaceEdge (dagOf m) rlist) (predOf m)
    nj <- njp (set moveSet)
    let uptd n prog = updateNode prog n
    np <- foldrIO uptd (mprime : ojp (set moveSet)) p
    let addnpi n prog = do
          s <- set <$> succNodes prog n
          foldrIO (\y prog' -> updateNode prog' (addPredNode y (nameOf n))) s prog
        addn n prog = addNode prog n >>= addnpi n
    foldrIO addn nj np

entries :: Program -> IO [Node]
entries p@(ns, n0, _) = do
  nl <- smExtract ns
  return (n0 : foldr (\a b -> if unreachable p a then a : b else b) [] nl)

addPredInfo :: Program -> IO Program
addPredInfo p@(ns, _, _) = do
  nl <- map (`setPredNode` makeSet) <$> smExtract ns
  np <- foldrIO (\n prog -> updateNode prog n) nl p
  let addpi n prog = do
        s <- set <$> succNodes prog n
        foldrIO (\sn prog' -> updateNode prog' (addPredNode sn (nameOf n))) s prog
  foldrIO addpi nl np

closure :: Program -> Node -> IO Program
closure p@(_, _, fn) entry = do
  let dfs prog _ [] = return prog
      dfs prog@(ns', _, _) parent (cur : todo) = do
        isin <- smIsin ns' (nameOf cur)
        if not isin
          then do
            prog' <- addNode prog cur
            np <- succNodes p cur >>= dfs prog' cur . set
            dfs np parent todo
          else dfs prog parent todo
  newNs <- smNew
  prog <- addNode (newNs, entry, fn) entry
  succNodes p entry >>= dfs prog entry . set

programs :: Program -> IO [Program]
programs p = do
  l <- addPredInfo p >>= entries
  mapM (\e -> closure p e >>= addPredInfo) l

------------------------------------------------------------------------
-- Compress

doMoveTests, doMoveOps :: Bool
doMoveTests = False
doMoveOps = True

filterSucc :: Program -> Name -> Set Name -> IO [Name]
filterSucc p nm fenceSet = do
  s <- set . succN p <$> nameToNode p nm
  return (foldr (\n l -> if member fenceSet n then l else n : l) [] s)

moveThingsNode :: Program -> Name -> Set Name -> IO Program
moveThingsNode p nm fenceSet = do
  let ntn = nameToNode
      sNmList prog = filterSucc prog nm fenceSet
  au <- listUnionEQ aeq <$> (sNmList p >>= mapM (fmap assignmentOf . ntn p))
  tu <- listUnionEQ teq <$> (sNmList p >>= mapM (fmap (testsOf . dagOf) . ntn p))
  let ms prog a = sNmList prog >>= \l ->
        foldrIO (\n acc -> ntn prog n >>= \nd -> return (if member (assignmentOf nd) a then n : acc else acc)) l []
      moveA1 a prog = do
        msl <- ms prog a
        msSet <- nameSetToNodeSet prog (listToSet msl)
        mpa <- ntn prog nm >>= moveOp prog a msSet
        foldrIO (\n prog' -> ntn prog' n >>= delete prog') msl mpa
      moveA a prog = if doMoveOps then moveA1 a prog else return prog
      moveT _ prog = if doMoveTests then error "move_test is not translated" else return prog
  foldrIO moveA (set au) p >>= foldrIO moveT (set tu)

moveThingsWindow :: Program -> Int -> Name -> Set Name -> IO Program
moveThingsWindow p w nm fenceSet = do
  node <- nameToNode p nm
  let things = numThingsNode node
  sNmList <- filterSucc p nm fenceSet
  childP <- if w > things
              then foldrIO (\n prog -> moveThingsWindow prog (w - things) n fenceSet) sNmList p
              else return p
  moveThingsNode childP nm fenceSet

cpress :: Int -> Program -> Set Name -> Set Name -> IO Program
cpress window p fenceSet everinFenceSet = do
  filled <- foldrIO (\nm prog -> moveThingsWindow prog window nm fenceSet) (set fenceSet) p
  succs <- mapM (\nm -> succN filled <$> nameToNode filled nm) (set fenceSet)
  let nfenceSet = listUnion (makeSet : succs)
      fFenceSet = listToSet (foldr (\a l -> if member everinFenceSet a then l else a : l) [] (set nfenceSet))
      nEverinFc = foldr (\a s -> add s a) everinFenceSet (set fFenceSet)
  if not (emptySet fFenceSet) then cpress window filled fFenceSet nEverinFc else return filled

cleanUp :: Program -> IO Program
cleanUp p@(_, n0, _) = do
  clos@(ns, _, _) <- closure p n0
  l <- map nameOf <$> smExtract ns
  foldrIO (\n prog -> nameToNode prog n >>= delete prog) l clos

compress :: Int -> Program -> IO Program
compress window p@(_, n0, _) = do
  let fenceSet = add makeSet (nameOf n0)
      everinFenceSet = add (makeEQ namePrefixEq) (nameOf n0)
  cpress window p fenceSet everinFenceSet >>= cleanUp

------------------------------------------------------------------------
-- ReadI

btarget :: [CodeTypes] -> Int -> (Int -> Int)
btarget [] _ = \_ -> error "BTARGET"
btarget (h : t) n =
  let rf = btarget t (n + 1)
      g lbl x = if lbl == x then n else rf x
  in case h of
       TARGET lbl _ -> g lbl
       _ -> rf

buildNodes :: [CodeTypes] -> IO Program
buildNodes l = do
  let t = btarget l 0
      f [] _ = []
      f (ci : rest) n =
        let nm = smlInt n
            nxtnm = smlInt (n + 1)
            asn i = listToSetEQ aeq i
            edag = reach dagMake (NAME nxtnm)
            tgtnm tgt = smlInt (t tgt)
            edagt tgt = reach dagMake (NAME (tgtnm tgt))
            finDag = reach dagMake (NAME (nameOf fin))
            cdag tgt tst = attach tst (edagt tgt) edag
            g = case ci of
              ASSIGNMENT i -> N nm (asn [i]) edag makeSet
              NERGLE -> N nm (asn []) edag makeSet
              LABELREF tgt (GETLAB _ dst) -> N nm (asn [GETLAB (t tgt, tgtnm tgt) dst]) edag makeSet
              COMPARISON tgt tst -> N nm (asn []) (cdag tgt tst) makeSet
              FLOW tgt _ -> N nm (asn []) (edagt tgt) makeSet
              EXIT i -> N nm (asn [i]) finDag makeSet
              TARGET _ _ -> N nm (asn []) edag makeSet
              _ -> error "BNODES"
        in g : fin : f rest (n + 1)
  makeProg >>= foldrIO (\n p -> addNode p n) (fin : f l (0 :: Int))

readI :: [Opcode] -> IO ([Opcode], [Program])
readI ol = do
  let cl = map classify ol
      junkfil x (junk, other) = case x of
        JUNK a -> (JUNK a : junk, other)
        _ -> (junk, x : other)
      (junk, other) = foldr junkfil ([], []) cl
      unjunk (JUNK i) = i
      unjunk _ = error "READI"
  p@(ns, _, fn) <- buildNodes other
  n0 <- nameToNode p "0"
  progs <- programs (ns, n0, fn)
  return (map unjunk junk, progs)

nameInt :: String -> Int
nameInt l = f 0 l
  where f n [] = n
        f n (d : r) = if d >= '0' && d <= '9' then f (n * 10 + ord d - ord '0') r else n

buildLiveTab :: Program -> IO (Name -> IO (Set Int))
buildLiveTab p@(_, n0, _) = do
  mr <- readIORef maxregRef
  lt <- smNew
  let fil a b = if a < 0 || isBogusReg mr (a, "") then b else add b a
      filLset s = foldr fil makeSet (set s)
      finset = listToSet [0, 1, 2, 3, 4, 5]
      flive f n = smMap lt (nameOf n) >>= maybe (f n) return
      dfs cur = do
        let nm = nameOf cur
            gen = filLset (readNode cur)
            kill = writeNode cur
        smAdd lt (nm, gen)
        children <- succNodes p cur
        chLive <- if emptySet children then return finset
                  else listUnion <$> mapM (flive dfs) (set children)
        let live = union (diff chLive kill) gen
        smRm lt nm
        smAdd lt (nm, live)
        return live
  _ <- dfs n0
  return (\nm -> fromMaybe (error "Stringmap") <$> smMap lt nm)

branchLive :: Program -> (Name -> IO (Set Int)) -> Name -> IO (Set Int)
branchLive p tab nm = do
  s <- set . succN p <$> nameToNode p nm
  listUnion <$> mapM tab s

writeP :: (Name -> Int) -> IO Int -> Program -> IO [Opcode]
writeP entryMap lblFun p@(ns, n0, fn) = do
  let blblmap [] = return (\x -> error ("blblmap_" ++ x))
      blblmap (nm : t) = do
        mp <- blblmap t
        mylab <- lblFun
        return (\x -> if x == nm then mylab else mp x)
  lblmap <- smExtract ns >>= blblmap . map nameOf
  liveTab <- buildLiveTab p
  let labelList nm = map (\r -> (r, "")) . set <$> liveTab nm
      brList nm = map (\r -> (r, "")) . set <$> branchLive p liveTab nm
      getlab (GETLAB (_, s) dst) = GETLAB (entryMap s, "node" ++ s) dst
      getlab _ = error "WRITEP1"
      dogetlabs i@(GETLAB _ _) l = getlab i : l
      dogetlabs i l = i : l
      ubranch frm nm = BRANCH Ieq (0, "zero") (0, "zero") (lblmap nm, "node" ++ nm) <$> brList frm
      cbranch (BRANCH test src1 src2 _ _) frm nm = BRANCH test src1 src2 (lblmap nm, "node" ++ nm) <$> brList frm
      cbranch _ _ _ = error "cbranch"
      label nm = LABEL (lblmap nm, "node" ++ nm) <$> labelList nm
      entryLabel nm = LABEL (entryMap nm, "entry") <$> labelList nm
      f done lastnm nm
        | nm == nameOf fn = return (done, [NOP])
        | member done nm = ubranch lastnm nm >>= \ub -> return (done, [NOP, ub])
        | otherwise = do
            node <- nameToNode p nm
            needlabel <- case set (predN p node) of
              [] -> return False
              [pn'] -> do
                pn <- nameToNode p pn'
                let d = dagOf pn
                    sel = selOf d
                    rt = rootOf d
                    istst (TEST _) = True
                    istst _ = False
                    untst (TEST t) = t
                    untst _ = error "needlabel1"
                return (istst rt && (sel (untst rt, True) == Just (NAME nm)))
              _ -> return True
            nodelabel <- if needlabel then (: []) <$> label nm else return []
            let a = foldr dogetlabs [] (set (assignmentOf node))
                d = dagOf node
                sel = selOf d
                rt = rootOf d
                unnm (Just (NAME x)) = x
                unnm _ = error "dag_code"
            (nl, cd) <- case rt of
              NEITHER -> return ([], [])
              NAME n -> return ([n], [])
              TEST t -> do
                let tN = unnm (sel (t, True))
                    fN = unnm (sel (t, False))
                cb <- cbranch t nm tN
                return ([fN, tN], [cb])
            let dfs dn [] = return (dn, [])
                dfs dn [h] = f dn nm h
                dfs dn [h, nxt] = do
                  (dn1, cd1) <- f dn nm h
                  (dn2, cd2) <- if member dn1 nxt then return (dn1, []) else dfs dn1 [nxt]
                  lbl <- if nxt == nameOf fn || member dn2 nxt then return [NOP]
                         else (\lb -> [NOP, lb]) <$> label nxt
                  return (dn2, cd1 ++ lbl ++ cd2)
                dfs _ _ = error "DFS_SURPRISE"
            (dn, dcd) <- dfs (add done nm) nl
            return (dn, NOP : nodelabel ++ a ++ cd ++ dcd)
  (_, code) <- f makeSet "badname" (nameOf n0)
  el <- entryLabel (nameOf n0)
  l0 <- label (nameOf n0)
  return (el : l0 : code)

writeI :: [Opcode] -> [Program] -> IO [Opcode]
writeI j p = do
  labelid <- newIORef 0
  let newlabel = modifyIORef' labelid (+ 1) >> subtract 1 <$> readIORef labelid
      bentrymap [] = return (\x -> error ("bentrymap_" ++ x))
      bentrymap ((_, n0, _) : t) = do
        mp <- bentrymap t
        mylab <- newlabel
        return (\x -> if x == nameOf n0 then mylab else mp x)
  entryMap <- bentrymap p
  let gt (_, n0a, _) (_, n0b, _) = nameInt (nameOf n0a) > nameInt (nameOf n0b)
      sp = sortBy' gt p
  i <- foldrIO (\a b -> (++ b) <$> writeP entryMap newlabel a) sp []
  return (i ++ j)

------------------------------------------------------------------------
-- Main

writeprog :: IORef [String] -> [Opcode] -> [Program] -> IO ()
writeprog out j p = writeI j p >>= rmBogus . remnops >>= showProg out

dodelay :: [Opcode] -> IO [Opcode]
dodelay i = delayInit i >> addDelay i

-- Main.doitx, with the input embedded and the two outputs in buffers
doitx :: [String] -> IORef [String] -> IORef [String] -> Int -> IO ()
doitx input ofile cOfile ws = do
  initNames
  i <- breaki (readAbs input) >>= dodelay
  (j, p) <- readI i
  writeprog ofile j p
  cp <- mapM (compress ws) p
  writeprog cOfile j cp
  _ <- newName "0"
  return ()

-- Main.main's argument parsing (print is a no-op)
mainArgs :: [String] -> [String] -> IORef [String] -> IORef [String] -> IO ()
mainArgs s input ofile cOfile = do
  idemp <- newIORef 0
  ws <- newIORef 0
  gotifile <- newIORef False
  gotofile <- newIORef False
  let digit d = if ord d >= ord '0' && ord d <= ord '9' then ord d - ord '0' else error ("expected digit. got " ++ [d])
      parse a = case a of
        ['-', 'i', 'd', 'e', 'm', d] -> writeIORef idemp (digit d)
        ['-', 'w', 's', d] -> writeIORef ws (digit d)
        '-' : _ -> error "exiting"
        _ -> do
          go <- readIORef gotofile
          gi <- readIORef gotifile
          unless go $ if gi then writeIORef gotofile True else writeIORef gotifile True
  mapM_ parse (tail s)
  readIORef idemp >>= writeIORef idempotency
  readIORef ws >>= doitx input ofile cOfile

round' :: Int -> IO String
round' k = do
  tmp <- newIORef []
  cmp <- newIORef []
  mainArgs ["foobar", "-ws9", "DATA/ndotprod.s", "DATA/tmp.s", "DATA/cmp.s"] (vary k ndotprod []) tmp cmp
  t <- concat . reverse <$> readIORef tmp
  c <- concat . reverse <$> readIORef cmp
  return ("== tmp.s\n" ++ t ++ "== cmp.s\n" ++ c)

main :: IO ()
main = do
  n <- iterations 35
  outs <- mapM round' [1 .. n]
  putStr (if all (== head outs) outs then head outs else "fail\n")

-- DATA/ndotprod.s (MLton benchmarks), one string per line
ndotprod :: [String]
ndotprod =
  [ "ARITHI  new_allocptr/R1 := zero/R0 iadd 257"
  , "LABEL   v/L0:      ( zero/R0 allocptr/R1 exnhandler/R2 std_closure/R3 std_arg/R4 std_cont/R5 )"
  , "GETLAB  v/R6 := v/L4"
  , "STORE   M[ allocptr/R1 + 0 ] := v/R6"
  , "STORE   M[ allocptr/R1 + 1 ] := std_cont/R5"
  , "ARITHI  closure/R5 := allocptr/R1 iadd 0"
  , "ARITHI  int10/R8 := zero/R0 iadd 10"
  , "GETREAL real3.0/R9 := 3.0"
  , "ARITHI  new_allocptr/R1 := allocptr/R1 iadd 2"
  , "BRANCH  IF zero/R0 ieq zero/R0 GOTO array/L1   ( zero/R0 allocptr/R1 exnhandler/R2 n/R8 v/R9 v/R5 )"
  , "LABEL   array/L1:      ( zero/R0 allocptr/R1 exnhandler/R2 n/R8 v/R9 v/R5 )"
  , "STORE   M[ allocptr/R1 + 0 ] := n/R8"
  , "ARITH   new_allocptr/R1 := n/R8 iadd allocptr/R1"
  , "ARITH   arr/R4 := allocptr/R1 isub n/R8"
  , "BRANCH  IF zero/R0 ige n/R8 GOTO else/L8   ( zero/R0 allocptr/R1 exnhandler/R2 arr/R4 v/R5 )"
  , "GETLAB  v/R6 := v/L3"
  , "STORE   M[ allocptr/R1 + 1 ] := v/R6"
  , "STORE   M[ allocptr/R1 + 2 ] := arr/R4"
  , "STORE   M[ allocptr/R1 + 3 ] := v/R5"
  , "ARITHI  closure/R5 := allocptr/R1 iadd 1"
  , "STORE   M[ arr/R4 + 1 ] := v/R9"
  , "ARITHI  int1/R6 := zero/R0 iadd 1"
  , "ARITHI  new_allocptr/R1 := allocptr/R1 iadd 4"
  , "BRANCH  IF zero/R0 ieq zero/R0 GOTO g/L2   ( zero/R0 allocptr/R1 exnhandler/R2 i/R6 v/R5 arr/R4 n/R8 v/R9 )"
  , "LABEL   else/L8:      ( zero/R0 allocptr/R1 exnhandler/R2 arr/R4 v/R5 )"
  , "FETCHi  v/R6 := M[ v/R5 + 0 ]"
  , "ARITHI  new_allocptr/R1 := allocptr/R1 iadd 1"
  , "JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "LABEL   g/L2:      ( zero/R0 allocptr/R1 exnhandler/R2 i/R6 v/R5 arr/R4 n/R8 v/R9 )"
  , "BRANCH  IF i/R6 ige n/R8 GOTO else/L9   ( zero/R0 allocptr/R1 exnhandler/R2 v/R5 )"
  , "ARITH   x/R7 := arr/R4 iadd i/R6"
  , "STORE   M[ x/R7 + 1 ] := v/R9"
  , "ARITHI  i/R6 := i/R6 iadd 1"
  , "BRANCH  IF i/R6 ige n/R8 GOTO else/L10   ( zero/R0 allocptr/R1 exnhandler/R2 v/R5 )"
  , "ARITH   x/R7 := arr/R4 iadd i/R6"
  , "STORE   M[ x/R7 + 1 ] := v/R9"
  , "ARITHI  i/R6 := i/R6 iadd 1"
  , "BRANCH  IF zero/R0 ieq zero/R0 GOTO g/L2   ( zero/R0 allocptr/R1 exnhandler/R2 i/R6 v/R5 arr/R4 n/R8 v/R9 )"
  , "LABEL   else/L10:      ( zero/R0 allocptr/R1 exnhandler/R2 v/R5 )"
  , "FETCHi  v/R6 := M[ v/R5 + 0 ]"
  , "MOVE    std_arg/R4 := zero/R0"
  , "JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "LABEL   else/L9:      ( zero/R0 allocptr/R1 exnhandler/R2 v/R5 )"
  , "FETCHi  v/R6 := M[ v/R5 + 0 ]"
  , "MOVE    std_arg/R4 := zero/R0"
  , "JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "LABEL   v/L3:      ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "FETCHi  arr/R4 := M[ std_cont/R5 + 1 ]"
  , "FETCHi  v/R5 := M[ std_cont/R5 + 2 ]"
  , "FETCHi  v/R6 := M[ v/R5 + 0 ]"
  , "JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "LABEL   v/L4:      ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "GETLAB  v/R6 := v/L5"
  , "STORE   M[ allocptr/R1 + 0 ] := v/R6"
  , "FETCHi  x/R6 := M[ std_cont/R5 + 1 ]"
  , "STORE   M[ allocptr/R1 + 1 ] := x/R6"
  , "STORE   M[ allocptr/R1 + 2 ] := std_arg/R4"
  , "ARITHI  closure/R5 := allocptr/R1 iadd 0"
  , "ARITHI  int10/R8 := zero/R0 iadd 10"
  , "GETREAL real4.0/R9 := 4.0"
  , "ARITHI  new_allocptr/R1 := allocptr/R1 iadd 3"
  , "BRANCH  IF zero/R0 ieq zero/R0 GOTO array/L1   ( zero/R0 allocptr/R1 exnhandler/R2 n/R8 v/R9 v/R5 )"
  , "LABEL   v/L5:      ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "GETLAB  v/R6 := v/L7"
  , "STORE   M[ allocptr/R1 + 0 ] := v/R6"
  , "FETCHi  x/R6 := M[ std_cont/R5 + 1 ]"
  , "STORE   M[ allocptr/R1 + 1 ] := x/R6"
  , "ARITHI  closure/R11 := allocptr/R1 iadd 0"
  , "GETREAL real0.0/R10 := 0.0"
  , "ARITHI  new_allocptr/R1 := allocptr/R1 iadd 2"
  , "MOVE    k/R8 := zero/R0"
  , "BRANCH  IF zero/R0 ieq zero/R0 GOTO f/L6   ( zero/R0 allocptr/R1 exnhandler/R2 k/R8 Q/R10 v/R11 X1/R4 closure/R5 )"
  , "LABEL   f/L6:      ( zero/R0 allocptr/R1 exnhandler/R2 k/R8 Q/R10 v/R11 X1/R4 closure/R5 )"
  , "ARITHI  int10/R6 := zero/R0 iadd 10"
  , "BRANCH  IF k/R8 ige int10/R6 GOTO else/L11   ( zero/R0 allocptr/R1 exnhandler/R2 v/R11 Q/R10 )"
  , "ARITHI  v/R9 := k/R8 iadd 1"
  , "FETCHi  Z1/R6 := M[ closure/R5 + 2 ]"
  , "ARITH   x/R6 := Z1/R6 iadd k/R8"
  , "FETCHm  v/R7 := M[ x/R6 + 1 ]"
  , "ARITH   x/R6 := X1/R4 iadd k/R8"
  , "FETCHm  v/R6 := M[ x/R6 + 1 ]"
  , "ARITH   v/R6 := v/R7 fmul v/R6"
  , "ARITH   v/R10 := Q/R10 fadd v/R6"
  , "MOVE    k/R8 := v/R9"
  , "BRANCH  IF zero/R0 ieq zero/R0 GOTO f/L6   ( zero/R0 allocptr/R1 exnhandler/R2 k/R8 Q/R10 v/R11 X1/R4 closure/R5 )"
  , "LABEL   else/L11:      ( zero/R0 allocptr/R1 exnhandler/R2 v/R11 Q/R10 )"
  , "FETCHi  v/R6 := M[ v/R11 + 0 ]"
  , "MOVE    std_cont/R5 := v/R11"
  , "MOVE    std_arg/R4 := Q/R10"
  , "JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "LABEL   v/L7:      ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  , "STORE   M[ allocptr/R1 + 0 ] := std_arg/R4"
  , "ARITHI  S/R4 := allocptr/R1 iadd 0"
  , "FETCHi  v/R5 := M[ std_cont/R5 + 1 ]"
  , "FETCHi  v/R6 := M[ v/R5 + 0 ]"
  , "ARITHI  new_allocptr/R1 := allocptr/R1 iadd 1"
  , "JUMP    v/R6   ( zero/R0 allocptr/R1 exnhandler/R2 std_cont/R5 std_arg/R4 )"
  ]
