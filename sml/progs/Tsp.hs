-- tsp.sml (MLton benchmarks, 3cps test/programs/from-mlton; AT&T Bell
-- Laboratories): a divide-and-conquer travelling-salesman heuristic over
-- 2097151 random points in a 2-d tree whose nodes are linked into cycles
-- through refs (node identity is ref identity).  Runs in IO.  Prints
-- testit's line (the cycle length with SML's Real.toString).
module Main (main) where

import Data.IORef
import Numeric (showFFloat)
import Repeat

data Tree = NULL
          | ND { left :: Tree, right :: Tree, x :: Double, y :: Double, sz :: Int
               , prev :: IORef Tree, next :: IORef Tree }

mkNode :: Tree -> Tree -> Double -> Double -> Int -> IO Tree
mkNode l r x' y' sz' = ND l r x' y' sz' <$> newIORef NULL <*> newIORef NULL

setPrev, setNext :: Tree -> Tree -> IO ()
setPrev t v = writeIORef (prev t) v
setNext t v = writeIORef (next t) v

link :: Tree -> Tree -> IO ()
link a b = writeIORef (next a) b >> writeIORef (prev b) a

sameNd :: Tree -> Tree -> Bool
sameNd a@(ND {}) b@(ND {}) = next a == next b
sameNd NULL NULL = True
sameNd _ _ = False

distance :: Tree -> Tree -> Double
distance (ND { x = ax, y = ay }) (ND { x = bx, y = by }) = sqrt ((ax - bx) * (ax - bx) + (ay - by) * (ay - by))
distance _ _ = error "distance"

-- Sling tree nodes into a list (the root is the list's tail; only next is set)
makeList :: Tree -> IO Tree
makeList NULL = return NULL
makeList t@(ND { left = l0, right = r0 }) = do
  l <- makeList l0
  r <- makeList r0
  retVal <- case (l, r) of
    (NULL, NULL) -> return t
    (ND {}, NULL) -> setNext l0 t >> return l
    (NULL, ND {}) -> setNext r0 t >> return r
    (ND {}, ND {}) -> setNext r0 t >> setNext l0 r >> return l
  writeIORef (next t) NULL
  return retVal

reverseT :: Tree -> IO ()
reverseT NULL = return ()
reverseT t = do
  p <- readIORef (prev t)
  setNext p NULL
  writeIORef (prev t) NULL
  nx <- readIORef (next t)
  rev t nx
  where rev _ NULL = return ()
        rev back tmp = do
          tmp' <- readIORef (next tmp)
          writeIORef (next tmp) back
          setPrev back tmp
          rev tmp tmp'

conquer :: Tree -> IO Tree
conquer NULL = return NULL
conquer t0 = do
  cycle <- makeList t0
  let loop NULL = return ()
      loop t = do
        doNext <- readIORef (next t)
        let findMinDist mn minDist tmp
              | sameNd cycle tmp = return mn
              | otherwise = do
                  let test = distance t tmp
                  nx <- readIORef (next tmp)
                  if test < minDist then findMinDist tmp test nx else findMinDist mn minDist nx
        cn <- readIORef (next cycle)
        mn <- findMinDist cycle (distance t cycle) cn
        minNext <- readIORef (next mn)
        minPrev <- readIORef (prev mn)
        let minToNext = distance mn minNext
            minToPrev = distance mn minPrev
            tToNext = distance t minNext
            tToPrev = distance t minPrev
        if tToPrev - minToPrev < tToNext - minToNext
          then link minPrev t >> link t mn
          else link mn t >> link t minNext
        loop doNext
  t' <- readIORef (next cycle)
  writeIORef (next cycle) cycle
  writeIORef (prev cycle) cycle
  loop t'
  return cycle

merge :: Tree -> Tree -> Tree -> IO Tree
merge a b t = do
  let locateCycle start = do
        let findMin mn minDist tmp
              | sameNd start tmp = return (mn, minDist)
              | otherwise = do
                  let test = distance t tmp
                  nx <- readIORef (next tmp)
                  if test < minDist then findMin tmp test nx else findMin mn minDist nx
        sn <- readIORef (next start)
        (mn, minDist) <- findMin start (distance t start) sn
        next' <- readIORef (next mn)
        prev' <- readIORef (prev mn)
        let minToNext = distance mn next'
            minToPrev = distance mn prev'
            tToNext = distance t next'
            tToPrev = distance t prev'
        return (if tToPrev - minToPrev < tToNext - minToNext
                  then (prev', tToPrev, mn, minDist)
                  else (mn, minDist, next', tToNext))
  (p1, tToP1, n1, tToN1) <- locateCycle a
  (p2, tToP2, n2, tToN2) <- locateCycle b
  let n1ToN2 = distance n1 n2
      n1ToP2 = distance n1 p2
      p1ToN2 = distance p1 n2
      p1ToP2 = distance p1 p2
      choose testChoice test (choice, minDist) = if test < minDist then (testChoice, test) else (choice, minDist)
      (choice, _) = choose 4 (tToN1 + tToN2 + p1ToP2)
                  $ choose 3 (tToN1 + tToP2 + p1ToN2)
                  $ choose 2 (tToP1 + tToN2 + n1ToP2)
                    (1 :: Int, tToP1 + tToP2 + n1ToN2)
  case choice of
    1 -> reverseT n2 >> link p1 t >> link t p2 >> link n2 n1
    2 -> link p1 t >> link t n2 >> link p2 n1
    3 -> link p2 t >> link t n1 >> link p1 n2
    _ -> reverseT n1 >> link n1 t >> link t n2 >> link p2 p1
  return t

tsp :: Tree -> Int -> IO Tree
tsp t@(ND { left = l, right = r, sz = sz' }) n
  | sz' <= n = conquer t
  | otherwise = do
      a <- tsp l n
      b <- tsp r n
      merge a b t
tsp NULL _ = return NULL

-- Park-Miller (Schrage's method), as Rand in the SML; m = 2^31 - 1
randM :: Int
randM = 2147483647

random' :: Int -> Int
random' seed =
  let a = 48271
      q = randM `div` a
      r = randM `mod` a
      hi = seed `div` q
      lo = seed `mod` q
      test = a * lo - r * hi
  in if test > 0 then test else test + randM

data Axis = XAxis | YAxis

buildTree :: Int -> IO Tree
buildTree n0 = do
  seed <- newIORef 314
  let drand48 = do
        modifyIORef' seed random'
        s <- readIORef seed
        return (fromIntegral s / fromIntegral randM)
      m_e12 = 162754.79141900392083592475
      median mn mx = do
        t <- drand48
        let retval = if t > 0.5
              then log (1.0 - (2.0 * (m_e12 - 1.0) * (t - 0.5) / m_e12)) / 12.0
              else negate (log (1.0 - (2.0 * (m_e12 - 1.0) * t / m_e12)) / 12.0)
        return (mn + ((retval + 1.0) * (mx - mn) / 2.0))
      uniform mn mx = (\r -> mn + r * (mx - mn)) <$> drand48
      build 0 _ _ _ _ _ = return NULL
      build n XAxis minX minY maxX maxY = do
        med <- median minY maxY
        l <- build (n `div` 2) YAxis minX minY maxX med
        r <- build (n `div` 2) YAxis minX med maxX maxY
        u <- uniform minX maxX
        mkNode l r u med n
      build n YAxis minX minY maxX maxY = do
        med <- median minX maxX
        l <- build (n `div` 2) XAxis minX minY med maxY
        r <- build (n `div` 2) XAxis med minY maxX maxY
        u <- uniform minY maxY
        mkNode l r med u n
  build n0 XAxis 0.0 0.0 1.0 1.0

-- SML's Real.toString (%.12g): 12 significant digits, trailing zeros
-- dropped, ~ for minus, E for the exponent
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

printLength :: Tree -> IO String
printLength NULL = return "(* 0 points *)"
printLength start = do
  let cyc t = case t of { ND {} -> next t == next start; _ -> False }
      dist ax ay bx by = let dx = ax - bx; dy = ay - by in sqrt (dx * dx + dy * dy)
      len NULL px py n l = return (n, l + dist px py (x start) (y start))
      len t px py n l
        | cyc t = return (n, l + dist px py (x start) (y start))
        | otherwise = readIORef (next t) >>= \nx -> len nx (x t) (y t) (n + 1) (l + dist px py (x t) (y t))
  nx <- readIORef (next start)
  if cyc nx then return "(* 1 point *)"
    else do
      (n, l) <- len nx (x start) (y start) (1 :: Int) 0.0
      return ("(* " ++ show n ++ "points, cycle length = " ++ smlReal l ++ " *)")

main :: IO ()
main = do
  n <- iterations 1
  outs <- mapM (\i -> buildTree (vary i 2097151 0) >>= \t -> tsp t 150 >>= printLength) [1 .. n]
  putStrLn (if all (== head outs) outs then head outs else "fail")
