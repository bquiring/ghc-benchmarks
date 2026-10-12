-- zern.sml (MLton benchmarks, 3cps test/programs/from-mlton; from David
-- McClain's language study): an E-field from 15 Zernike screens on 128x128
-- arrays of doubles (scaled sums, then cos and sin of every element),
-- through a 2-d real-array library (FastRealArray2) passing element
-- functions to array-wide loops.  Only the library functions ztest uses
-- are translated.  Like the SML, each round checks rpart(0,1) ~ 0.219;
-- the output is the sum of rpart (as tools/mlton/zern.sml prints it).
module Main (main) where

import Data.Array.IO
import Data.Array.Base (unsafeRead, unsafeWrite)
import Numeric (showFFloat)
import Repeat

data Array2 = Array2 { arrData :: IOUArray Int Double, nrows :: Int, ncols :: Int, nelts :: Int }

dotimes :: Int -> (Int -> IO ()) -> IO ()
dotimes n f = iter 0
  where iter k = if k >= n then return () else f k >> iter (k + 1)

maxLength :: Int
maxLength = 4096 * 4096

chkSize :: Int -> Int -> Int
chkSize nr nc
  | nr <= 0 || nc <= 0 = error "Size"
  | maxLength < n = error "Size"
  | otherwise = n
  where n = nr * nc

mkArray :: Int -> Double -> IO (IOUArray Int Double)
mkArray n v = do
  arr <- newArray (0, n - 1) 0.0
  dotimes n (\ix -> unsafeWrite arr ix v)
  return arr

array :: Int -> Int -> Double -> IO Array2
array nr nc v = do
  let n = chkSize nr nc
  d <- mkArray n v
  return (Array2 d nr nc n)

tabulateRM :: Int -> Int -> (Int -> Int -> Double) -> IO Array2
tabulateRM nr nc f = do
  let n = chkSize nr nc
  arr <- newArray (0, n - 1) 0.0
  let lp1 i k = if i < nr then lp2 i 0 k else return ()
      lp2 i j k = if j < nc then unsafeWrite arr k (f i j) >> lp2 i (j + 1) (k + 1) else lp1 (i + 1) k
  lp2 0 0 0
  return (Array2 arr nr nc n)

sub :: Array2 -> Int -> Int -> IO Double
sub a i j
  | i >= 0 && i < nrows a && j >= 0 && j < ncols a = unsafeRead (arrData a) (i * ncols a + j)
  | otherwise = error "Subscript"

foldRM :: (Double -> b -> b) -> b -> Array2 -> IO b
foldRM f ini (Array2 d _ _ n) = go 0 ini
  where go k acc = if k < n then unsafeRead d k >>= \v -> go (k + 1) (f v acc) else return acc

binop :: Array2 -> Array2 -> Array2 -> (Double -> Double -> Double) -> IO ()
binop dst src1 src2 f = dotimes (nelts dst) $ \ix -> do
  a <- unsafeRead (arrData src1) ix
  b <- unsafeRead (arrData src2) ix
  unsafeWrite (arrData dst) ix (f a b)

unop :: Array2 -> Array2 -> (Double -> Double) -> IO ()
unop dst src f = dotimes (nelts dst) $ \ix -> unsafeRead (arrData src) ix >>= unsafeWrite (arrData dst) ix . f

collect :: Int -> (Int -> a) -> [a]
collect n0 f = g n0 []
  where g 0 l = l
        g n l = g (n - 1) (f n : l)

ncoefs, nx, ny :: Int
ncoefs = 15
nx = 128
ny = nx

mulsv :: Array2 -> Double -> Array2 -> IO ()
mulsv dst sf a = unop dst a (\v -> sf * v)

cisv :: Array2 -> Array2 -> Array2 -> IO (Array2, Array2)
cisv a rpart ipart = unop rpart a cos >> unop ipart a sin >> return (rpart, ipart)

mpadd :: Array2 -> Double -> Array2 -> IO ()
mpadd dst sf src = binop dst dst src (\vdst vsrc -> vdst + sf * vsrc)

zern :: Array2 -> Array2 -> Array2 -> [Double] -> [Array2] -> IO (Array2, Array2)
zern dst rpart ipart coefs zerns = do
  mulsv dst (head coefs) (head zerns)
  sequence_ (zipWith (mpadd dst) (tail coefs) (tail zerns))
  cisv dst rpart ipart

-- One iteration of ztest: whether rpart(0,1) ~ 0.219, and the sum of rpart
iteration :: Int -> IO (Bool, Double)
iteration k = do
  s <- array ny nx 0.0
  rpart <- array ny nx 0.0
  ipart <- array ny nx 0.0
  let coefs = collect ncoefs (\x -> fromIntegral (1 + x))
  zerns <- sequence (collect ncoefs (\_ -> tabulateRM ny nx (\r c -> 0.01 * fromIntegral (nx * r + c + k))))
  (rp, _) <- zern s rpart ipart coefs zerns
  v <- sub rp 0 1
  total <- foldRM (+) 0.0 rp
  return (abs (v - 0.219) < 0.001, total)

-- SML's Real.toString (%.12g)
smlReal :: Double -> String
smlReal v
  | v < 0 = '~' : smlReal (negate v)
  | v == 0 = "0"
  | otherwise = trim (showFFloat (Just (11 - e)) v "")
  where e = floor (logBase 10 v) :: Int
        trim str = case break (== '.') str of
          (i, '.' : f) -> let f' = reverse (dropWhile (== '0') (reverse f)) in if null f' then i else i ++ "." ++ f'
          _ -> str

main :: IO ()
main = do
  n <- iterations 1000
  rs <- mapM (\i -> iteration (vary i 0 0)) [1 .. n]
  putStrLn (if all fst rs && all ((== snd (head rs)) . snd) rs then "sum " ++ smlReal (snd (head rs)) else "fail")
