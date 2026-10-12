-- barnes-hut.sml (MLton benchmarks, 3cps test/programs/from-mlton; the
-- SML/NJ suite, AT&T Bell Laboratories): a Barnes-Hut N-body simulation in
-- 3-d, a Plummer model of 512 bodies over 80 time steps, with bodies and
-- the oct-tree's nodes holding refs and cells as mutable arrays.  Runs in
-- IO.  The SML's benchmark prints nothing; this prints what the driver in
-- tools/mlton/barnes-hut.sml prints: the total number of interactions and
-- the sum of the squared final positions (SML's Real.toString).
module Main (main) where

import Data.Array.IO
import Data.Bits (shiftR, (.&.))
import Data.IORef
import Data.Maybe (fromMaybe)
import Numeric (showFFloat)
import System.IO.Unsafe (unsafePerformIO)
import Repeat

------------------------------------------------------------------------
-- Vector3

data V = V { vx :: Double, vy :: Double, vz :: Double }

tabulateV :: Monad m => (Int -> m Double) -> m V
tabulateV f = do { x <- f 0; y <- f 1; z <- f 2; return (V x y z) }

zerov :: V
zerov = V 0.0 0.0 0.0

equalV :: V -> V -> Bool
equalV (V x y z) (V x1 y1 z1) = x == x1 && y == y1 && z == z1

addv, subv :: V -> V -> V
addv (V x1 y1 z1) (V x2 y2 z2) = V (x1 + x2) (y1 + y2) (z1 + z2)
subv (V x1 y1 z1) (V x2 y2 z2) = V (x1 - x2) (y1 - y2) (z1 - z2)

dotvp :: V -> V -> Double
dotvp (V x1 y1 z1) (V x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

addvs, mulvs, divvs :: V -> Double -> V
addvs (V x y z) s = V (x + s) (y + s) (z + s)
mulvs (V x y z) s = V (x * s) (y * s) (z * s)
divvs (V x y z) s = V (x / s) (y / s) (z / s)

mapv :: (Double -> a) -> V -> (a, a, a)
mapv f (V x y z) = (f x, f y, f z)

map3v :: (Double -> Double -> Double -> Double) -> V -> V -> V -> V
map3v f (V x1 y1 z1) (V x2 y2 z2) (V x3 y3 z3) = V (f x1 x2 x3) (f y1 y2 y3) (f z1 z2 z3)

-- foldv f {x, y, z} init = f(z, f(y, f(x, init)))
foldv :: (a -> b -> b) -> (a, a, a) -> b -> b
foldv f (x, y, z) initial = f z (f y (f x initial))

------------------------------------------------------------------------
-- Space

data Body = Body { mass :: Double, pos :: IORef V, vel :: IORef V, acc :: IORef V, phi :: IORef Double }

data Cell = BodyCell Body | Cell (IOArray Int Node)

data Node = Empty | Node { nmass :: IORef Double, npos :: IORef V, cell :: Cell }

data Space = Space { rmin :: V, rsize :: Double, root :: Node }

eqBody :: Body -> Body -> IO Bool
eqBody (Body m p v a h) (Body m1 p1 v1 a1 h1) = do
  ph <- readIORef h; ph1 <- readIORef h1
  pp <- readIORef p; pp1 <- readIORef p1
  vv <- readIORef v; vv1 <- readIORef v1
  aa <- readIORef a; aa1 <- readIORef a1
  return (m == m1 && ph == ph1 && equalV pp pp1 && equalV vv vv1 && equalV aa aa1)

nsub :: Int
nsub = 8

putCell :: Cell -> Int -> Node -> IO ()
putCell (Cell a) i nd = writeArray a i nd
putCell (BodyCell _) _ _ = error "putCell"

getCell :: Cell -> Int -> IO Node
getCell (Cell a) i = readArray a i
getCell (BodyCell _) _ = error "getCell"

mkCell :: IO Cell
mkCell = Cell <$> newArray (0, nsub - 1) Empty

mkBodyNode :: Body -> IO Node
mkBodyNode body = do
  m <- newIORef (mass body)
  p <- readIORef (pos body) >>= newIORef
  return (Node m p (BodyCell body))

mkCellNode :: Cell -> IO Node
mkCellNode c = do
  m <- newIORef 0.0
  p <- newIORef zerov
  return (Node m p c)

------------------------------------------------------------------------
-- Load

imax :: Int
imax = 0x20000000

imaxrs1 :: Int
imaxrs1 = imax `shiftR` 1

rIMAX :: Double
rIMAX = fromIntegral imax

-- Integerized coordinates; Nothing for the SML's NotIntCoord
intcoord :: V -> V -> Double -> Maybe (Int, Int, Int)
intcoord rp rmin' rsize' =
  let xsc = divvs (subv rp rmin') rsize'
      cvt x = if 0.0 <= x && x < 1.0 then Just (floor (rIMAX * x)) else Nothing
  in case mapv cvt xsc of
       (Just a, Just b, Just c) -> Just (a, b, c)
       _ -> Nothing

notIntCoord :: Maybe a -> a
notIntCoord = fromMaybe (error "NotIntCoord")

subindex :: (Int, Int, Int) -> Int -> Int
subindex iv l = fst (foldv aux iv (0, 0))
  where aux v (i, k) = if (v .&. l) /= 0 then (i + (nsub `shiftR` (k + 1)), k + 1) else (i, k + 1)

expandBox :: Body -> Space -> IO Space
expandBox nd box@(Space rmin' rsize' root') = do
  p <- readIORef (pos nd)
  case intcoord p rmin' rsize' of
    Just _ -> return box
    Nothing -> do
      let rmid = addvs rmin' (0.5 * rsize')
          rmin'' = map3v (\x y z -> if x < y then z - rsize' else z) p rmid rmin'
          rsize'' = 2.0 * rsize'
          mksub v r = do
            let x = notIntCoord (intcoord v rmin'' rsize'')
                k = subindex x imaxrs1
            c <- mkCell
            putCell c k r
            return c
      box' <- case root' of
        Empty -> return (Space rmin'' rsize'' root')
        _ -> Space rmin'' rsize'' <$> (mksub rmid root' >>= mkCellNode)
      expandBox nd box'

loadTree :: Body -> Space -> IO Space
loadTree body (Space rmin' rsize' root') = do
  posp <- readIORef (pos body)
  let xp = notIntCoord (intcoord posp rmin' rsize')
      insert Empty _ = mkBodyNode body
      insert n@(Node { cell = BodyCell _, npos = posq }) l = do
        q <- readIORef posq
        let xq = notIntCoord (intcoord q rmin' rsize')
            k = subindex xq l
        a <- mkCell
        putCell a k n
        mkCellNode a >>= \n' -> insert n' l
      insert n@(Node { cell = c }) l = do
        let k = subindex xp l
        subtree <- getCell c k >>= \sub -> insert sub (l `shiftR` 1)
        putCell c k subtree
        return n
  Space rmin' rsize' <$> insert root' imaxrs1

hackCofM :: Node -> IO ()
hackCofM Empty = return ()
hackCofM (Node { cell = BodyCell _ }) = return ()
hackCofM (Node { cell = Cell subcells, nmass = mass', npos = pos' }) = sumMass 0 0.0 zerov
  where
    sumMass i totMass cofm =
      if i < nsub
        then readArray subcells i >>= \sub -> case sub of
          Empty -> sumMass (i + 1) totMass cofm
          nd@(Node { nmass = m', npos = p' }) -> do
            hackCofM nd
            m <- readIORef m'
            p <- readIORef p'
            sumMass (i + 1) (totMass + m) (addv cofm (mulvs p m))
        else do
          writeIORef mass' totMass
          writeIORef pos' (divvs cofm totMass)

makeTree :: [Body] -> V -> Double -> IO Space
makeTree bodies rmin' rsize' = do
  let build [] space = return space
      build (body : r) space =
        if mass body == 0.0 then build r space
        else expandBox body space >>= loadTree body >>= build r
  space <- build bodies (Space rmin' rsize' Empty)
  hackCofM (root space)
  return space

------------------------------------------------------------------------
-- Grav

data Walk = Walk { wphi0 :: Double, wacc0 :: V, nbcterm :: Int, n2bterm :: Int, skip :: Bool }

walk :: V -> Double -> V -> Body -> Double -> Double -> Double -> Node -> IO Walk
walk acc0' phi0' pos0 pskip eps rsize' tol root' = do
  skipSelf <- newIORef False
  nbc <- newIORef (0 :: Int)
  n2b <- newIORef (0 :: Int)
  let tolsq = tol * tol
      gravsub Empty phi0 acc0 _ = return (phi0, acc0)
      gravsub (Node { nmass = mass', npos = pos', cell = c }) phi0 acc0 memo = do
        (dr, drsq) <- case memo of
          Nothing -> readIORef pos' >>= \p -> let dr = subv p pos0 in return (dr, dotvp dr dr + eps * eps)
          Just (dr', drsq') -> return (dr', drsq' + eps * eps)
        m <- readIORef mass'
        let phii = m / sqrt drsq
        case c of
          Cell _ -> modifyIORef' nbc (+ 1)
          _ -> modifyIORef' n2b (+ 1)
        return (phi0 - phii, addv acc0 (mulvs dr (phii / drsq)))
      walksub p dsq phi0 acc0 = case p of
        Empty -> return (phi0, acc0)
        Node { cell = BodyCell body } -> do
          eq <- eqBody body pskip
          if eq then writeIORef skipSelf True >> return (phi0, acc0)
          else gravsub p phi0 acc0 Nothing
        Node { cell = Cell a, npos = pos' } -> do
          pp <- readIORef pos'
          let dr = subv pp pos0
              drsq = dotvp dr dr
          if tolsq * drsq < dsq
            then do
              let loop i phi0'' acc0'' =
                    if i < nsub
                      then do
                        sub <- readArray a i
                        (phi0''', acc0''') <- walksub sub (dsq / 4.0) phi0'' acc0''
                        loop (i + 1) phi0''' acc0'''
                      else return (phi0'', acc0'')
              loop 0 phi0 acc0
            else gravsub p phi0 acc0 (Just (dr, drsq))
  (phi0, acc0) <- walksub root' (rsize' * rsize') phi0' acc0'
  Walk phi0 acc0 <$> readIORef nbc <*> readIORef n2b <*> readIORef skipSelf

hackGrav :: Body -> Node -> Double -> Double -> Double -> IO (Int, Int, Bool)
hackGrav body root' rsize' eps tol = do
  p <- readIORef (pos body)
  w <- walk zerov 0.0 p body eps rsize' tol root'
  writeIORef (phi body) (wphi0 w)
  writeIORef (acc body) (wacc0 w)
  return (n2bterm w, nbcterm w, skip w)

------------------------------------------------------------------------
-- Rand and Main

randA, randM :: Double
randA = 16807.0
randM = 2147483647.0

random' :: Double -> Double
random' seed = let t = randA * seed in t - randM * fromIntegral (floor (t / randM) :: Int)

seedRef :: IORef Double
seedRef = unsafePerformIO (newIORef 0.0)
{-# NOINLINE seedRef #-}

srand :: Int -> IO ()
srand s = writeIORef seedRef (fromIntegral s)

xrand :: Double -> Double -> IO Double
xrand xl xh = do
  r <- random' <$> readIORef seedRef
  writeIORef seedRef r
  return (xl + (((xh - xl) * r) / 2147483647.0))

piD :: Double
piD = 3.14159265358979323846

pow :: Double -> Double -> Double
pow x y = if y == 0.0 then 1.0 else exp (y * log x)

pickshell :: Double -> IO V
pickshell rad = pickvec
  where pickvec = do
          vec <- tabulateV (\_ -> xrand (-1.0) 1.0)
          let rsq = dotvp vec vec
          if rsq > 1.0 then pickvec else return (mulvs vec (rad / sqrt rsq))

testdata :: Int -> IO [Body]
testdata n = mkBodies n zerov zerov []
  where
    mfrac = 0.999
    rn = fromIntegral n
    rsc = (3.0 * piD) / 16.0
    vsc = sqrt (1.0 / rsc)
    mkBodies 0 cmr cmv l = do
      let cmr' = divvs cmr rn
          cmv' = divvs cmv rn
          norm [] l' = return l'
          norm (p : r) l' = do
            modifyIORef (pos p) (`subv` cmr')
            modifyIORef (vel p) (`subv` cmv')
            norm r (p : l')
      norm l []
    mkBodies i cmr cmv l = do
      xr <- xrand 0.0 mfrac
      let r = 1.0 / sqrt (pow xr (-2.0 / 3.0) - 1.0)
      pos' <- pickshell (rsc * r)
      let vN = do
            x <- xrand 0.0 1.0
            y <- xrand 0.0 0.1
            if y > x * x * pow (1.0 - x * x) 3.5 then vN else return x
      vn <- vN
      let v = (sqrt 2.0 * vn) / pow (1.0 + r * r) 0.25
      vel' <- pickshell (vsc * v)
      body <- Body (1.0 / rn) <$> newIORef pos' <*> newIORef vel' <*> newIORef zerov <*> newIORef 0.0
      mkBodies (i - 1) (addv cmr pos') (addv cmv vel') (body : l)

-- Advance the system one time step
stepSystem :: ((Int, Int, Int) -> IO ()) -> [Body] -> Double -> Double -> Int -> V -> Double -> Double -> Double -> IO (Int, Double)
stepSystem output plist dtime eps nstep rmin' rsize' tnow tol = do
  let dthf = 0.5 * dtime
  Space _ rsize'' root' <- makeTree plist rmin' rsize'
  let recalc [] n2bcalc nbccalc selfint = return (n2bcalc, nbccalc, selfint)
      recalc (p : r) n2bcalc nbccalc selfint = do
        acc1 <- readIORef (acc p)
        (n2bterm', nbcterm', skipSelf) <- hackGrav p root' rsize'' eps tol
        if nstep > 0
          then do
            a <- readIORef (acc p)
            modifyIORef (vel p) (\v -> addv v (mulvs (subv a acc1) dthf))
          else return ()
        recalc r (n2bcalc + n2bterm') (nbccalc + nbcterm') (if skipSelf then selfint else selfint + 1)
      advance (Body { pos = pos', acc = acc', vel = vel' }) = do
        dvel <- (`mulvs` dthf) <$> readIORef acc'
        vel1 <- (`addv` dvel) <$> readIORef vel'
        let dpos = mulvs vel1 dtime
        modifyIORef pos' (`addv` dpos)
        writeIORef vel' (addv vel1 dvel)
  calcs <- recalc plist 0 0 0
  output calcs
  mapM_ advance plist
  return (nstep + 1, tnow + dtime)

go :: ((Int, Int, Int) -> IO ()) -> [Body] -> Double -> Double -> Double -> Double -> Double -> V -> Double -> IO ()
go output bodies tnow0 tstop dtime eps tol rmin' rsize' = loop 0 tnow0
  where loop nstep tnow =
          if tnow < tstop + 0.1 * dtime
            then stepSystem output bodies dtime eps nstep rmin' rsize' tnow tol >>= uncurry loop
            else return ()

-- SML's Real.toString (%.12g)
smlReal :: Double -> String
smlReal v
  | v < 0 = '~' : smlReal (negate v)
  | v == 0 = "0.0"
  | e < -4 || e >= 12 = let (m, ex) = sci in m ++ "E" ++ (if ex < 0 then '~' : show (negate ex) else show ex)
  | otherwise = trim (showFFloat (Just (11 - e)) v "")
  where
    e = floor (logBase 10 v) :: Int
    sci = let m = v / 10 ^^ e in (trim (showFFloat (Just 11) m ""), e)
    trim str = case break (== '.') str of
      (i, '.' : f) -> let f' = reverse (dropWhile (== '0') (reverse f)) in if null f' then i else i ++ "." ++ f'
      _ -> str

-- The driver's run: total interactions, sum of squared final positions
run :: Int -> IO String
run n = do
  srand 123
  bodies <- testdata n
  terms <- newIORef (0 :: Int)
  rmin' <- tabulateV (\_ -> return (-2.0))
  go (\(n2b, nbc, _) -> modifyIORef' terms (+ (n2b + nbc))) bodies 0.0 2.0 0.025 0.05 1.0 rmin' 4.0
  ps <- mapM (readIORef . pos) bodies
  t <- readIORef terms
  return (show t ++ " " ++ smlReal (foldl (\s p -> s + dotvp p p) 0.0 ps))

main :: IO ()
main = do
  n <- iterations 1
  outs <- mapM (\i -> run (vary i 512 0)) [1 .. n]
  putStrLn (if all (== head outs) outs then head outs else "fail")
