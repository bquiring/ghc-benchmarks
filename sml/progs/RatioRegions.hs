-- ratio-regions.sml (3cps test/programs/llvm-sml; Jeffrey Siskind's Scheme
-- code, translated to SML by Stephen Weeks): Ratio Regions image
-- segmentation, a reduction to max flow solved by preflow-push with
-- periodic relabeling, on a 50x50 image, repeated.
--
-- Imperative, like the original: matrices are mutable arrays (IOUArray),
-- the relabeling queue is a linked list of IORefs.  The SML's print is a
-- no-op; here pormat's lines (push/lift/relabel counts) are printed, and
-- the size of the minimum cut, for checking against MLton
-- (tools/mlton/ratio-regions.sml).
module Main (main) where

import Control.Monad
import Data.Array.IO
import Data.IORef
import Repeat

doo :: Int -> (Int -> IO ()) -> IO ()
doo mx f = loop 0
  where loop i = when (i < mx) (f i >> loop (i + 1))

everyN :: Int -> (Int -> IO Bool) -> IO Bool
everyN n p = loop 0
  where loop i = if i >= n then return True else p i >>= \b -> if b then loop (i + 1) else return False

type Matrix a = (Int, Int, IOUArray (Int, Int) a)

makeMatrix :: MArray IOUArray a IO => Int -> Int -> a -> IO (Matrix a)
makeMatrix m n a = do { arr <- newArray ((0, 0), (m - 1, max 0 (n - 1))) a; return (m, n, arr) }

matrixRows, matrixColumns :: Matrix a -> Int
matrixRows (m, _, _) = m
matrixColumns (_, n, _) = n

mref :: MArray IOUArray a IO => Matrix a -> Int -> Int -> IO a
mref (_, _, a) i j = readArray a (i, j)

mset :: MArray IOUArray a IO => Matrix a -> Int -> Int -> a -> IO ()
mset (_, _, a) i j x = writeArray a (i, j) x

data PormatValue = PInt Int | PString String

-- Format with ~s (an int), ~a (a string) and ~% (newline)
pormat :: (String -> IO ()) -> String -> [PormatValue] -> IO ()
pormat say control values0 = say (go control values0)
  where go [] _ = []
        go ('~' : 's' : r) (PInt n : vs) = show n ++ go r vs
        go ('~' : 'a' : r) (PString s : vs) = s ++ go r vs
        go ('~' : '%' : r) vs = go r vs        -- a line ends; say adds it
        go (c : r) vs = c : go r vs

positiveMin, positiveMinus, positivePlus :: Int -> Int -> Int
positiveMin x y = if x < 0 then y else min x y
positiveMinus x y = if x < 0 then x else x - y
positivePlus x y = if x < 0 then x else x + y

-- The relabeling queue: a linked list with mutable tails
data Queue a = Nil | Cons a (IORef (Queue a))

raoRatioRegion :: (String -> IO ()) -> Matrix Int -> Matrix Int -> Matrix Int -> Int -> IO [[Bool]]
raoRatioRegion say cRight cDown w lgMaxV = do
  let height = matrixRows w
      width = matrixColumns w
  fRight <- makeMatrix height (width - 1) 0
  fDown <- makeMatrix (height - 1) width 0
  fT <- makeMatrix height width 0
  h <- makeMatrix height width 0
  e <- makeMatrix height width 0
  marked <- makeMatrix height width False
  let m1 = height * width + 2
      m2 = 2 * height * width + 2
  q <- newArray (0, 2 * height * width + 2) [] :: IO (IOArray Int [(Int, Int)])
  let cfRight y x = (-) <$> mref cRight y x <*> mref fRight y x
      cfLeft y x = (+) <$> mref cRight y (x - 1) <*> mref fRight y (x - 1)
      cfDown y x = (-) <$> mref cDown y x <*> mref fDown y x
      cfUp y x = (+) <$> mref cDown (y - 1) x <*> mref fDown (y - 1) x
      efRight y x = (> 0) <$> cfRight y x
      efLeft y x = (> 0) <$> cfLeft y x
      efDown y x = (> 0) <$> cfDown y x
      efUp y x = (> 0) <$> cfUp y x
      andM [] = return True
      andM (m : ms) = m >>= \b -> if b then andM ms else return False
      orM [] = return False
      orM (m : ms) = m >>= \b -> if b then return True else orM ms
      preflowPush v = do
        let enqueue y x = do
              mk <- mref marked y x
              unless mk $ do
                hh <- mref h y x
                old <- readArray q hh
                writeArray q hh ((x, y) : old)
                mset marked y x True
            cfT y x = (\wv f -> v * wv - f) <$> mref w y x <*> mref fT y x
            efT y x = (> 0) <$> cfT y x
            nonzeroE y x = (/= 0) <$> mref e y x
            hEq y x y' x' = (\a b -> a == b + 1) <$> mref h y x <*> mref h y' x'
            canPushRight y x = andM [return (x < width - 1), nonzeroE y x, efRight y x, hEq y x y (x + 1)]
            canPushLeft y x = andM [return (x > 0), nonzeroE y x, efLeft y x, hEq y x y (x - 1)]
            canPushDown y x = andM [return (y < height - 1), nonzeroE y x, efDown y x, hEq y x (y + 1) x]
            canPushUp y x = andM [return (y > 0), nonzeroE y x, efUp y x, hEq y x (y - 1) x]
            canPushT y x = andM [nonzeroE y x, efT y x, (== 1) <$> mref h y x]
            hLe y x y' x' = (<=) <$> mref h y x <*> mref h y' x'
            canLift y x = andM
              [ nonzeroE y x
              , if x == width - 1 then (<= m1) <$> mref h y x
                else orM [not <$> efRight y x, hLe y x y (x + 1)]
              , if x == 0 then (<= m1) <$> mref h y x
                else orM [not <$> efLeft y x, hLe y x y (x - 1)]
              , if y == height - 1 then (<= m1) <$> mref h y x
                else orM [not <$> efDown y x, hLe y x (y + 1) x]
              , if y == 0 then (<= m1) <$> mref h y x
                else orM [not <$> efUp y x, hLe y x (y - 1) x]
              , orM [not <$> efT y x, (== 0) <$> mref h y x] ]
            pushRight y x = do
              ee <- mref e y x; cf <- cfRight y x
              let df = positiveMin ee cf
              mref fRight y x >>= \f -> mset fRight y x (f + df)
              mset e y x (positiveMinus ee df)
              mref e y (x + 1) >>= \e2 -> mset e y (x + 1) (positivePlus e2 df)
              enqueue y (x + 1)
            pushLeft y x = do
              ee <- mref e y x; cf <- cfLeft y x
              let df = positiveMin ee cf
              mref fRight y (x - 1) >>= \f -> mset fRight y (x - 1) (f - df)
              mset e y x (positiveMinus ee df)
              mref e y (x - 1) >>= \e2 -> mset e y (x - 1) (positivePlus e2 df)
              enqueue y (x - 1)
            pushDown y x = do
              ee <- mref e y x; cf <- cfDown y x
              let df = positiveMin ee cf
              mref fDown y x >>= \f -> mset fDown y x (f + df)
              mset e y x (positiveMinus ee df)
              mref e (y + 1) x >>= \e2 -> mset e (y + 1) x (positivePlus e2 df)
              enqueue (y + 1) x
            pushUp y x = do
              ee <- mref e y x; cf <- cfUp y x
              let df = positiveMin ee cf
              mref fDown (y - 1) x >>= \f -> mset fDown (y - 1) x (f - df)
              mset e y x (positiveMinus ee df)
              mref e (y - 1) x >>= \e2 -> mset e (y - 1) x (positivePlus e2 df)
              enqueue (y - 1) x
            pushT y x = do
              ee <- mref e y x; cf <- cfT y x
              let df = positiveMin ee cf
              mref fT y x >>= \f -> mset fT y x (f + df)
              mset e y x (positiveMinus ee df)
            lift y x = do
              a <- if x == width - 1 then return m1 else efRight y x >>= \b -> if b then mref h y (x + 1) else return m2
              b <- if x == 0 then return m1 else efLeft y x >>= \c -> if c then mref h y (x - 1) else return m2
              c <- if y == height - 1 then return m1 else efDown y x >>= \d -> if d then mref h (y + 1) x else return m2
              d <- if y == 0 then return m1 else efUp y x >>= \f -> if f then mref h (y - 1) x else return m2
              t <- efT y x
              mset h y x (1 + minimum [a, b, c, d, if t then 0 else m2])
            relabel = do
              rq <- newIORef Nil
              tl <- newIORef Nil
              let isNull r = readIORef r >>= \qq -> return (case qq of { Nil -> True; _ -> False })
                  enq y x value = do
                    hh <- mref h y x
                    when (value < hh) $ do
                      mset h y x value
                      mk <- mref marked y x
                      unless mk $ do
                        mset marked y x True
                        t <- readIORef tl
                        case t of
                          Nil -> do { r <- newIORef Nil; writeIORef tl (Cons (x, y) r); readIORef tl >>= writeIORef rq }
                          Cons _ cdr -> do { r <- newIORef Nil; writeIORef cdr (Cons (x, y) r); readIORef cdr >>= writeIORef tl }
                  deq = readIORef rq >>= \qq -> case qq of
                    Nil -> error "dequeue"
                    Cons p rest -> do
                      mset marked (snd p) (fst p) False
                      readIORef rest >>= writeIORef rq
                      nl <- isNull rq
                      when nl (writeIORef tl Nil)
                      return p
              doo height $ \y -> doo width $ \x -> mset h y x m1 >> mset marked y x False
              doo height $ \y -> doo width $ \x -> do
                t <- efT y x
                hh <- mref h y x
                when (t && hh > 1) (enq y x 1)
              let loop = do
                    nl <- isNull rq
                    unless nl $ do
                      (x, y) <- deq
                      value <- (+ 1) <$> mref h y x
                      b1 <- if x > 0 then efRight y (x - 1) else return False
                      when b1 (enq y (x - 1) value)
                      b2 <- if x < width - 1 then efLeft y (x + 1) else return False
                      when b2 (enq y (x + 1) value)
                      b3 <- if y > 0 then efDown (y - 1) x else return False
                      when b3 (enq (y - 1) x value)
                      b4 <- if y < height - 1 then efUp (y + 1) x else return False
                      when b4 (enq (y + 1) x value)
                      loop
              loop
        doo height $ \y -> doo width $ \x -> mset e y x 0 >> mset fT y x 0
        doo height $ \y -> doo (width - 1) $ \x -> mset fRight y x 0
        doo (height - 1) $ \y -> doo width $ \x -> mset fDown y x 0
        doo height $ \y -> mset e y (width - 1) (-1) >> mset e y 0 (-1)
        doo (width - 1) $ \x -> mset e (height - 1) x (-1) >> mset e 0 x (-1)
        pushes <- newIORef (0 :: Int)
        lifts <- newIORef (0 :: Int)
        relabels <- newIORef (0 :: Int)
        let plural r one more = readIORef r >>= \k -> return (PInt k, PString (if k == 1 then one else more))
            report suffix i = do
              (p1, p2) <- plural pushes "" "es"
              (l1, l2) <- plural lifts "" "s"
              (r1, r2) <- plural relabels "" "s"
              pormat say ("~s push~a, ~s lift~a, ~s relabel~a, ~s wave~a" ++ suffix ++ "~%")
                     [p1, p2, l1, l2, r1, r2, PInt i, PString (if i == 1 then "" else "s")]
            hi = 2 * height * width + 2   -- the queue's last index
            loop i p
              | i `mod` 6 == 0 && not p = do
                  relabel
                  modifyIORef' relabels (+ 1)
                  done <- everyN height $ \y -> everyN width $ \x ->
                            (\ee hh -> ee == 0 || hh == m1) <$> mref e y x <*> mref h y x
                  if done
                    then report ", terminated early" i
                    else do
                      doo (hi + 1) $ \k -> writeArray q k []
                      doo height $ \y -> doo width $ \x -> mset marked y x False
                      doo height $ \y -> doo width $ \x -> do
                        nz <- (/= 0) <$> mref e y x
                        when nz (enqueue y x)
                      loop i True
              | otherwise = do
                  any' <- someVector $ \ps -> orM [ orM [canPushRight y x, canPushLeft y x, canPushDown y x,
                                                         canPushUp y x, canPushT y x, canLift y x]
                                                  | (x, y) <- ps ]
                  if any'
                    then do
                      let inner k = when (k >= 0) $ do
                            ps <- readArray q k
                            writeArray q k []
                            forM_ ps $ \(x, y) -> mset marked y x False
                            forM_ ps $ \(x, y) -> do
                              let step can act cnt = can y x >>= \b -> when b (modifyIORef' cnt (+ 1) >> act y x)
                              step canPushRight pushRight pushes
                              step canPushLeft pushLeft pushes
                              step canPushDown pushDown pushes
                              step canPushUp pushUp pushes
                              step canPushT pushT pushes
                              step canLift lift lifts
                              nz <- (/= 0) <$> mref e y x
                              when nz (enqueue y x)
                            inner (k - 1)
                      inner hi
                      loop (i + 1) False
                    else do
                      relabel
                      modifyIORef' relabels (+ 1)
                      report "" i
            someVector p = go 0
              where go k = if k > hi then return False
                           else readArray q k >>= p >>= \b -> if b then return True else go (k + 1)
        loop 0 False
      minCutIncludesEveryEdgeToT =
        everyN height $ \y -> everyN width $ \x -> (== m1) <$> mref h y x
      minCut = forM [0 .. height - 1] $ \y -> forM [0 .. width - 1] $ \x -> (/= m1) <$> mref h y x
      outer lgV vMax
        | lgV < 0 = do
            pormat say "V-MAX=~s~%" [PInt vMax]
            preflowPush (vMax + 1)
            minCut
        | otherwise = do
            let v = vMax + (let lp i c = if i == 0 then c else lp (i - 1) (c + c) in lp lgV 1)
            pormat say "LG-V=~s, V-MAX=~s, V=~s~%" [PInt lgV, PInt vMax, PInt v]
            preflowPush v
            every <- minCutIncludesEveryEdgeToT
            outer (lgV - 1) (if every then v else vMax)
  outer lgMaxV 0

doit :: (String -> IO ()) -> Int -> IO [[Bool]]
doit say n = do
  let height = n
      width = n
      lgMaxV = 15
  cRight <- makeMatrix height (width - 1) (-1)
  cDown <- makeMatrix (height - 1) width (-1)
  doo height $ \y -> doo (width - 1) $ \x ->
    mset cRight y x (if y >= height `quot` 4 && y < (3 * height) `quot` 4
                        && (x == width `quot` 4 - 1 || x == (3 * width) `quot` 4 - 1)
                     then 1 else 128)
  doo (height - 1) $ \y -> doo width $ \x ->
    mset cDown y x (if x >= width `quot` 4 && x < (3 * width) `quot` 4
                       && (y == height `quot` 4 - 1 || y == (3 * height) `quot` 4 - 1)
                    then 1 else 128)
  w <- makeMatrix height width 1
  raoRatioRegion say cRight cDown w lgMaxV

-- One round: its output lines, and the size of the cut
round' :: Int -> IO [String]
round' i = do
  buf <- newIORef []
  cut <- doit (\s -> modifyIORef' buf (s :)) (vary i 50 0)
  out <- readIORef buf
  return (reverse out ++ ["cut size " ++ show (length (filter id (concat cut)))])

main :: IO ()
main = do
  n <- iterations 12
  first <- round' 1
  same <- allM (\i -> (== first) <$> round' i) [2 .. n]
  mapM_ putStrLn (if same then first else ["fail: rounds differ"])
  where allM _ [] = return True
        allM p (x : xs) = p x >>= \b -> if b then allM p xs else return False
