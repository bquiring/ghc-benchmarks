-- smith-normal-form.sml (MLton benchmarks, 3cps test/programs/from-mlton;
-- "snf"): the Smith normal form of a 35x35 integer matrix with big-integer
-- (IntInf, here Integer) arithmetic, on a mutable matrix accessed through
-- row and column closures.  Like the SML, each elimination step prints
-- "dd: pos = N"; one round's lines are printed.  The SML checks the last
-- diagonal entry, as does this.
module Main (main) where

import Data.Array.IO
import Data.IORef
import Repeat

-- A matrix: height, width, entries row by row
data Matrix e = Matrix Int Int (IOArray Int e)

make :: Int -> Int -> (Int -> Int -> e) -> IO (Matrix e)
make h w gen = Matrix h w <$> newListArray (0, h * w - 1) [ gen (z `div` w) (z `mod` w) | z <- [0 .. h * w - 1] ]

height, width :: Matrix e -> Int
height (Matrix h _ _) = h
width (Matrix _ w _) = w

checkIndex :: Bool -> IO ()
checkIndex ok = if ok then return () else ioError (userError "index")

fetch :: Matrix e -> Int -> Int -> IO e
fetch (Matrix h w a) row col = checkIndex (0 <= row && row < h && 0 <= col && col < w) >> readArray a (col + w * row)

fetchRow :: Matrix e -> Int -> Int -> IO e
fetchRow (Matrix h w a) row = let offset = w * row in
  \col -> checkIndex (0 <= row && row < h && 0 <= col && col < w) >> readArray a (col + offset)

fetchCol :: Matrix e -> Int -> Int -> IO e
fetchCol (Matrix h w a) col = \row -> checkIndex (0 <= col && col < w && 0 <= row && row < h) >> readArray a (col + w * row)

storeRow :: Matrix e -> Int -> Int -> e -> IO ()
storeRow (Matrix h w a) row = let offset = w * row in
  \col e -> checkIndex (0 <= row && row < h && 0 <= col && col < w) >> writeArray a (col + offset) e

storeCol :: Matrix e -> Int -> Int -> e -> IO ()
storeCol (Matrix h w a) col = \row e -> checkIndex (0 <= col && col < w && 0 <= row && row < h) >> writeArray a (col + w * row) e

swapLoop :: (Int -> IO e) -> (Int -> e -> IO ()) -> (Int -> IO e) -> (Int -> e -> IO ()) -> Int -> IO ()
swapLoop from1 to1 from2 to2 limit = loop 0
  where loop i = if i == limit then return () else do
          tmp <- from1 i
          from2 i >>= to1 i
          to2 i tmp
          loop (i + 1)

rowSwap :: Matrix e -> Int -> Int -> IO ()
rowSwap mat row1 row2 = do
  checkIndex (0 <= row1 && row1 < height mat && 0 <= row2 && row2 < height mat)
  if row1 == row2 then return ()
    else swapLoop (fetchRow mat row1) (storeRow mat row1) (fetchRow mat row2) (storeRow mat row2) (width mat)

colSwap :: Matrix e -> Int -> Int -> IO ()
colSwap mat col1 col2 = do
  checkIndex (0 <= col1 && col1 < width mat && 0 <= col2 && col2 < width mat)
  if col1 == col2 then return ()
    else swapLoop (fetchCol mat col1) (storeCol mat col1) (fetchCol mat col2) (storeCol mat col2) (height mat)

opLoop :: (Int -> IO e) -> (Int -> IO e) -> (Int -> e -> IO ()) -> Int -> (e -> e -> e) -> IO ()
opLoop from1 from2 to2 limit f = loop 0
  where loop i = if i == limit then return () else do
          a <- from1 i
          b <- from2 i
          to2 i (f a b)
          loop (i + 1)

rowOp :: Matrix e -> Int -> Int -> (e -> e -> e) -> IO ()
rowOp mat row1 row2 f = do
  checkIndex (0 <= row1 && row1 < height mat && 0 <= row2 && row2 < height mat && row1 /= row2)
  opLoop (fetchRow mat row1) (fetchRow mat row2) (storeRow mat row2) (width mat) f

colOp :: Matrix e -> Int -> Int -> (e -> e -> e) -> IO ()
colOp mat col1 col2 f = do
  checkIndex (0 <= col1 && col1 < width mat && 0 <= col2 && col2 < width mat && col1 /= col2)
  opLoop (fetchCol mat col1) (fetchCol mat col2) (storeCol mat col2) (height mat) f

copy :: Matrix e -> IO (Matrix e)
copy (Matrix h w a) = do
  es <- getElems a
  Matrix h w <$> newListArray (0, h * w - 1) es

mapM' :: Matrix e1 -> (e1 -> e2) -> IO (Matrix e2)
mapM' (Matrix h w a) f = do
  es <- getElems a
  Matrix h w <$> newListArray (0, h * w - 1) (map f es)

smaller :: Integer -> Integer -> Bool
smaller a b = a /= 0 && (b == 0 || abs a < abs b)

smithNormalForm :: (String -> IO ()) -> Matrix Integer -> IO (Matrix Integer)
smithNormalForm say mat0 = do
  mat <- copy mat0
  let h = height mat
      w = width mat
      range = min w h
      dd pos = do
        let matCol = fetchCol mat pos
            matRow = fetchRow mat pos
        say ("dd: pos = " ++ show pos)
        let swapRowLoop best bestRow bestCol row
              | row >= h = rowSwap mat pos bestRow >> colSwap mat pos bestCol
              | otherwise =
                  let mr = fetchRow mat row
                      swapColLoop best' bestRow' bestCol' col
                        | col >= w = swapRowLoop best' bestRow' bestCol' (row + 1)
                        | otherwise = do
                            next <- mr col
                            if smaller next best'
                              then swapColLoop next row col (col + 1)
                              else swapColLoop best' bestRow' bestCol' (col + 1)
                  in swapColLoop best bestRow bestCol pos
            rowLoop row
              | row < h = do
                  v <- matCol row
                  if v == 0 then rowLoop (row + 1) else do
                    p <- matCol pos
                    let x = negate (v `quot` p)
                    rowOp mat pos row (\lhs rhs -> lhs * x + rhs)
                    v' <- matCol row
                    if v' == 0 then rowLoop (row + 1) else hitPosAgain
              | otherwise = colLoop (pos + 1)
            colLoop col
              | col < w = do
                  v <- matRow col
                  if v == 0 then colLoop (col + 1) else do
                    p <- matRow pos
                    let x = negate (v `quot` p)
                    colOp mat pos col (\lhs rhs -> lhs * x + rhs)
                    v' <- matRow col
                    if v' == 0 then colLoop (col + 1) else hitPosAgain
              | otherwise = return ()
            hitPosAgain = swapRowLoop 0 pos pos pos >> rowLoop (pos + 1)
        hitPosAgain
      loop pos = if pos == range then return mat else dd pos >> loop (pos + 1)
  loop 0

-- One round: the lines it printed, and whether the last entry is right
snf :: Int -> IO ([String], Bool)
snf k = do
  out <- newIORef []
  let dim = 35 + k
  m <- make dim dim (\x y -> (table !! x) !! y)
  big <- mapM' m toInteger
  res <- smithNormalForm (\s -> modifyIORef' out (s :)) big
  entry <- fetch res (dim - 1) (dim - 1)
  ls <- readIORef out
  return (reverse ls, entry == -1027954043102083189860753402541358641712697245)

main :: IO ()
main = do
  n <- iterations 12
  rs <- mapM (\i -> snf (vary i 0 0)) [1 .. n]
  let first = fst (head rs)
  mapM_ putStrLn (if all snd rs && all ((== first) . fst) rs then first else ["fail"])

table :: [[Int]]
table =
  [ [8, -3, 1, 3, 6, 9, -2, 4, -9, -9, 2, 3, 8, -1, 3, -5, 4, -3, -5, -6, 8, 1, 4, -5, 7, -4, -4, -7, 7, 1, 4, -3, 8, 4, -4, -8, 5, -9, 3, -4, 1, 9, -8, -6, -2, 8, -9, -5, -3, -3]
  , [0, 8, -6, -2, -3, 4, 5, -2, 7, -7, -6, -7, -3, -4, 9, 7, -3, 3, 0, 3, 3, -8, -8, 2, 3, 8, 3, -2, -4, 3, -6, -6, -2, 6, 5, -1, -3, 1, 8, -8, 2, 1, -7, -7, -7, -3, -6, 6, -4, -9]
  , [0, -5, 8, -9, 2, 4, 2, 7, -4, 9, -3, 6, -2, 3, -3, 0, -9, 5, 8, -1, 2, -8, 3, 4, -6, 5, -6, -5, -8, 0, -5, 3, -2, -5, 8, 7, -1, 1, -1, 7, 6, 3, 6, 5, 6, 8, 7, 9, 7, -3]
  , [5, 4, 7, 2, 3, -9, 7, -7, 3, -8, 7, 5, 5, -2, -6, -3, 6, 5, 3, -1, -1, 4, 5, -5, 5, 9, 9, 3, 8, -3, -1, 9, -9, 6, -7, 7, 4, 6, -8, -9, 0, -3, -2, -7, 1, -2, -6, 7, 7, 7]
  , [2, 9, 9, 3, -4, 0, 9, 2, 5, 3, -5, -3, -1, 1, 8, -6, 2, -4, -8, -7, -8, 4, 5, 8, -1, -1, 7, 2, 5, 5, -4, -7, -3, -7, 6, -4, -5, -8, -5, -9, -8, 5, -5, -5, 0, 8, 8, 6, 4, -1]
  , [5, 5, 1, -7, 3, -5, 4, 9, 3, 4, 4, -5, 7, -1, 7, 4, -7, 7, -7, -2, 9, -9, 0, -4, -4, 0, 2, 6, 3, -1, 6, 6, 8, -6, -4, -9, 3, -2, -5, 5, -3, 2, -1, -6, 9, 3, -3, -8, -9, 7]
  , [7, 1, 2, 7, 6, 5, -6, -3, -4, -8, 0, 9, 6, 1, 2, -5, 4, 4, 4, -6, -7, -9, -6, 2, -4, 5, -2, 1, 0, 1, -8, 7, -7, -5, 4, 1, -5, 4, -4, -2, -3, 1, 1, 3, 4, -4, -5, 9, 8, -2]
  , [6, 2, -1, -8, 4, -7, 7, -3, -2, -5, 3, 0, 3, -9, 3, 3, 9, -1, 4, 8, -9, 6, -5, 9, 5, -1, -1, -9, 7, -2, 3, 9, 8, 9, 2, 7, 7, 6, -1, -1, -2, -2, -7, 3, -6, 0, -9, 4, 3, 7]
  , [0, -6, -3, -7, -1, 5, -2, 8, -5, -3, -8, 7, -2, -2, 0, -8, 4, 8, 9, -5, -4, -8, -1, 7, 1, 1, 6, -9, -4, 0, 8, 4, 3, -7, 6, 0, 1, 8, 6, -1, -1, -7, 9, -9, -5, -2, -2, -1, 1, 0]
  , [-4, 9, 6, -3, -2, -6, -3, 4, 8, -8, 1, -5, 9, 7, 9, 7, -9, -6, 6, 1, -3, 3, -3, -7, 1, 7, -7, 0, -2, 7, -4, -6, 0, 1, -3, -5, -9, -7, 8, 4, 9, -8, -8, -7, -6, 7, 6, -3, -8, 5]
  , [6, 7, -5, -9, 6, 1, 8, 4, -2, 7, -7, -1, -9, 1, -6, -5, 4, 9, 6, 0, -8, -3, 1, -3, 8, -3, 2, 9, -3, -9, -1, -3, 4, 3, 2, -9, -5, -3, 8, -4, 8, 5, -4, 7, 6, -8, 7, 6, -5, 5]
  , [1, 7, -8, -9, -7, -3, 8, 9, -7, -1, -7, 4, 0, 0, 1, -5, 9, -8, -1, -2, 3, 5, 9, -9, 5, 4, -9, 1, -4, -2, 3, -4, 8, -6, -4, -8, -5, -5, 4, -2, -4, -1, -9, -5, 2, -9, 2, -9, -2, -3]
  , [-5, -4, -4, 9, 2, 7, -2, 6, 7, 2, -9, 4, 2, 7, 8, -9, 2, 5, 3, 9, 6, 3, 0, -7, -6, -7, 6, -2, 9, -3, -6, 9, -9, 2, 2, -6, -1, 4, -3, 3, 0, 6, -3, 4, 9, 9, -6, 5, 5, -5]
  , [5, -7, 8, -4, 8, 8, -4, -9, 6, 0, -3, 6, 0, 8, 8, -6, -2, 5, 4, -1, -8, 1, -3, -1, 2, 3, -9, -9, -5, 1, 8, -5, -3, 0, -4, -9, 0, -6, 3, -1, -7, 0, 8, 9, -6, -1, -9, 1, -6, 2]
  , [7, -5, -1, 5, -2, 7, 0, -7, -1, 8, 8, -3, 9, -5, 7, -8, -8, -4, 3, 2, -1, 8, -2, 1, 2, 5, 0, -6, 7, 3, 3, 7, -5, 5, -1, 1, 0, -8, 1, 0, 0, -4, 6, 9, -5, -6, 3, -5, 8, 5]
  , [-4, -2, 3, -3, -1, 2, -2, -1, -9, -5, 1, 0, 0, 2, 9, -3, -9, 2, 9, 3, 8, -3, 4, 8, 8, 3, -3, -1, -4, 4, -6, -9, 5, -2, 1, 3, -7, -5, -6, -5, -8, 4, -8, -3, 5, 0, 7, -9, 6, 2]
  , [5, 1, 4, -3, -1, -9, 5, -8, -8, 6, 1, 1, -2, 7, 5, 6, -4, 2, -7, 0, -7, -3, -5, 9, 3, 4, -6, 8, -4, 3, 6, 0, 2, 3, -6, 3, 9, 4, 1, -4, 6, -5, -7, 0, -1, -8, -3, -9, 9, 7]
  , [2, -6, -1, 8, 4, -3, -1, -6, -2, -8, -2, -1, -1, -5, -9, -8, 9, -9, 5, 1, 9, -1, -6, 9, -7, 2, 8, -7, 4, -9, 7, 6, -2, 1, -2, -7, 8, 0, 5, 0, -5, -7, -6, 0, 4, 0, 3, -8, 5, 4]
  , [-2, 9, -9, -6, 1, -8, 8, 4, -6, 8, 1, -3, -7, 8, -5, 2, -8, 1, 3, -2, 6, 6, 6, 1, 0, 0, -7, 7, -3, -3, 0, -4, 3, -7, -6, 7, 5, 9, -5, 7, -8, 2, 3, -8, -7, 6, -5, -5, -8, -9]
  , [-7, -4, 4, 1, -1, -3, -8, 3, 7, 9, 8, 3, 0, 4, 4, -1, -5, 4, 2, 2, 0, 6, -6, 2, -9, 8, -9, 3, -2, 2, 6, 6, 1, 7, 1, 0, -8, 2, 3, -3, 8, 9, 5, 5, -6, 4, -7, -4, -2, -3]
  , [-5, 8, 6, 1, -6, -6, 6, 1, 1, -3, -9, -6, 2, -7, 2, -1, 6, -6, 0, 2, -7, 8, -8, 4, 9, -3, 9, -7, -9, -6, -4, -4, -5, 8, 2, -5, -4, -3, 5, 2, 1, -3, -3, -7, -9, 3, 7, -7, 3, -8]
  , [-4, -7, -2, 2, -4, -2, 6, -3, -1, -4, 0, -5, 9, 7, -6, -9, 7, -9, -6, 2, -3, 1, 5, -9, 4, -5, 4, -9, 1, -2, -2, 4, 0, 4, -8, -8, 3, -1, -5, -4, -9, -7, 7, 6, 3, -9, 6, 4, -4, -7]
  , [-9, 6, 6, -5, -1, -7, 4, -9, 4, -1, 6, -4, 7, 2, 8, 7, 3, 1, -7, 7, 7, 9, 8, -9, 7, 2, 1, 2, -8, 4, 5, 6, 7, 2, -7, 6, 8, 4, -9, 7, -5, 6, 9, -1, 9, 2, 0, 9, 3, 6]
  , [4, -3, 8, 0, -2, -2, 2, -3, 8, 3, 1, -8, -5, -2, 5, 6, 8, 0, -3, 4, -2, 4, -9, -5, 7, 6, -4, -7, 2, 4, -3, -8, -9, 9, 8, -9, 3, -7, 4, -7, -5, 4, 9, 3, -6, -3, -7, 4, 2, -2]
  , [-8, -8, 6, -2, -6, 8, -3, 3, -1, -7, 1, 9, 1, 7, -6, 8, -2, -9, -1, 3, -4, 7, 8, -1, 9, -9, 6, -3, 5, 0, 2, 5, -1, -6, -6, 1, 8, 6, -3, -9, -1, 9, -2, 9, -8, -7, -3, 6, -3, -3]
  , [5, -2, 3, 0, -9, -8, -6, 1, 8, 0, 1, 2, -8, -2, 0, -9, -8, 0, 5, -3, -4, 5, 6, -2, -5, 0, -9, 9, -9, -5, 9, 9, -5, -2, 4, 3, 8, -8, -7, 5, -3, -2, 2, 3, 9, 7, -1, 0, 4, -1]
  , [-4, 5, -5, 7, 8, 9, 7, -3, 1, 9, -7, -1, 8, -5, -1, 2, -8, 1, 0, 9, -8, -1, 6, -1, 9, -8, 7, 4, -8, 7, 0, -6, 2, 3, 7, 4, -3, -5, 9, -3, 0, 6, -9, 2, 4, -8, 6, -7, 9, 1]
  , [7, 0, -9, 6, 8, 2, 2, 5, -6, -6, 9, -5, 9, 2, 2, -8, 0, -6, -9, -6, -4, -9, 8, -2, 9, 7, -5, -1, 7, 2, -7, 7, -1, -3, 6, 6, 1, -4, 0, -1, -6, -5, 6, -7, -3, -2, 8, 2, -9, 8]
  , [8, -7, -9, -6, 9, -7, -7, 6, -8, 9, 5, -4, 1, -7, -8, -6, -3, 8, -8, 1, -8, 6, 9, -3, -7, 7, 1, 6, 1, 0, 8, -5, -8, 8, -9, 0, 4, 4, 3, -4, 6, -3, -9, 0, 4, -4, -5, -9, -5, -8]
  , [-3, -2, 8, 1, -1, -1, -4, 3, 7, -2, -9, 9, -8, -9, 6, -4, 7, -1, -5, -3, -9, 0, -3, 0, 7, 9, 1, -2, 7, -9, -6, 3, 3, -4, -7, -3, -4, -8, -2, -3, -9, -2, -6, 3, -6, -4, 7, -5, -8, -1]
  , [-9, -9, -2, -9, -9, 9, 6, 6, 7, 5, -1, -2, 1, 5, 2, -3, -4, 1, -6, 0, -3, -9, -1, 7, 0, -9, 5, -2, -2, 5, 3, 4, -1, 6, -6, 3, -6, 7, -1, 5, -8, -4, -2, -2, -6, -5, -6, 3, -1, 4]
  , [7, 7, 8, 7, 6, 1, -2, 5, -6, 9, 4, 8, 5, 0, -4, -2, -2, -5, -2, -6, 9, -8, -2, -5, -9, 3, -6, -3, -4, -5, -2, 6, 1, 6, -5, 0, -3, -2, 4, -6, 1, 6, -1, 3, -9, 2, -3, 1, 5, -6]
  , [6, 4, -7, 3, -7, 9, 1, -7, -8, 0, -6, 8, 4, 1, 9, 6, 8, 3, 0, 9, 0, 4, 9, -7, -7, 1, 5, 1, -5, 6, 9, 2, 4, 1, -9, 8, 4, 5, 8, 3, 2, -9, -6, -9, 9, -9, 7, -6, -4, 3]
  , [-3, -9, -4, 2, 3, 9, -9, 8, -9, 9, -4, -9, -5, 5, 0, 7, 3, -5, -8, 2, -3, 0, -9, -3, 1, 9, 4, 5, -1, 8, 0, -4, -2, 9, -4, -1, 3, 5, 9, -1, 1, 4, -8, -2, -3, 5, 1, 5, -6, 7]
  , [9, -3, 2, -9, 3, 4, 0, 7, -5, 9, 0, -6, 7, -2, 3, -7, 2, -5, -2, 6, 3, -9, -5, -9, 5, 2, -5, -3, 8, -5, 6, 2, 9, -7, -7, -7, -6, 9, -3, 6, 0, 6, -6, -9, 4, -3, -9, 0, -4, -9]
  , [-4, -8, 8, -7, 7, 0, -6, -6, 8, -9, -4, 5, -3, -1, 7, -5, -6, -1, 8, 6, -2, 1, -1, 5, -9, 1, -1, -7, -6, -6, -6, -4, 6, 3, -5, -5, -6, 2, 3, -6, -8, -3, 8, -2, -5, -4, -3, 1, 4, -4]
  , [4, -6, 2, 6, 2, -8, 8, 5, 8, -2, 0, -6, -1, -6, -2, 2, 6, -9, -7, -6, -4, -4, -7, -2, 8, 6, 3, -7, -6, 8, 2, 3, 4, 5, 3, 4, -6, 8, 8, -1, 4, -5, 6, 2, 8, -3, -9, -2, 6, 7]
  , [3, -4, 0, -3, -5, 0, -2, -6, -2, 8, 5, -9, -4, -8, -6, 0, 8, 9, 1, -2, 8, 2, -2, 8, 9, 3, 3, 5, -9, -3, -2, 7, 2, 9, 0, 4, 8, -9, 0, -6, 9, -9, 9, -4, 8, -8, -8, 2, -3, 2]
  , [-1, 3, -9, -8, -7, 6, -6, 3, 0, 5, -5, 1, 2, -2, -3, 7, 7, 3, -4, -2, -9, -5, -1, 9, 6, 8, 2, 8, 7, -3, 4, 6, 6, 0, -2, 2, -7, -7, 6, -3, 8, 2, 1, 0, 8, -1, 3, 9, 8, 6]
  , [1, -2, -3, 6, 5, 5, -6, -4, -5, 1, 1, 6, -7, -4, -3, 4, 4, -8, -9, 7, -2, -3, -7, -2, 1, 2, 0, 8, -6, -5, -5, 7, 8, 5, -2, 3, 9, 0, 5, 1, 3, -4, -6, 1, 4, -9, -2, 5, 4, 3]
  , [3, 3, 9, -2, 6, 9, 4, 9, 4, -8, 5, -1, 3, -2, 1, -7, -3, 2, 2, 0, -3, 3, 8, 2, 0, -5, 7, 1, 4, -8, 8, -9, -1, 1, -9, -4, 5, 2, 2, 8, 6, 1, 6, -2, 2, 7, 1, -6, -1, -1]
  , [4, -2, 4, -1, -5, -1, 5, -2, 3, -4, -5, 0, 2, -4, 6, 4, -3, 2, 2, 5, -6, -7, -9, -1, -9, -9, 6, 0, 6, 5, 9, -1, 3, -3, -8, 8, -8, 8, 4, 5, -1, -5, 1, 0, 3, -2, 5, 6, 6, 5]
  , [-4, 9, 6, 8, -9, 5, 5, -3, -7, 7, 6, 8, -8, 0, 4, -1, 9, 5, -7, 0, -1, -2, 3, 6, 0, 4, -3, 1, 4, 6, 4, 0, 5, -1, 7, -7, -6, -8, -3, -6, 7, -1, -3, -2, -3, -5, 3, 1, -8, -9]
  , [-6, 4, -5, 9, 9, -7, -1, -8, -4, 2, -6, 0, -6, -6, 7, 6, 0, 1, 7, -7, 0, -4, -6, -8, -9, 5, -6, -9, 2, -7, -2, -6, 9, 4, -5, 0, 4, -4, -5, 6, 9, 1, -6, -5, 3, -1, 7, -7, -6, 7]
  , [-8, 7, 7, -6, 7, -4, 8, 0, -9, -8, -3, 7, -3, 3, 8, -7, -2, -7, 5, 5, -5, 4, 6, 2, 4, 1, 4, -9, -3, 8, 8, -9, -4, -2, 1, -3, 1, 3, 9, -5, -8, -2, 7, 8, 9, 2, 0, 1, -9, 6]
  , [-7, 1, -9, 5, -5, -5, 7, 6, -5, -9, -6, -8, -6, 9, 7, 9, 0, -5, 7, 7, -6, 4, 5, -9, -1, -2, -7, 3, -5, -2, -5, 5, -3, -4, -2, -8, 2, -8, 0, -8, 0, -8, 9, 8, -5, -5, 1, 3, 5, -4]
  , [-8, -8, 0, -5, -8, -6, 3, -6, -4, 6, 1, -5, -6, -8, -4, -6, -2, -6, 6, -4, 8, 8, 4, -5, -1, 0, 9, -8, -3, -1, -8, 7, -3, 0, -7, 1, -7, -1, -7, 3, -7, 3, -4, -8, 8, -7, -9, -8, 3, 2]
  , [3, 6, 8, -9, 7, 1, -9, 9, 3, 8, 6, 4, -2, 1, -8, 4, -7, -4, -3, 3, -5, -6, -7, -2, 0, -4, 5, 2, 5, 6, 3, -8, 2, -5, -7, 6, 8, -2, -5, -4, 9, 9, 2, -2, -2, 7, 4, 4, -2, 3]
  , [6, 6, -5, -2, -8, -2, -9, 0, 2, 4, -6, -9, 9, 0, -8, -3, -1, -2, -1, 6, 8, 2, -9, 5, -2, 1, 7, -6, 5, 1, -1, 4, -4, -7, -6, -3, -8, 2, 2, 5, 5, -6, 5, 3, 3, 7, 4, 7, -3, -9]
  , [-9, 6, -4, 1, 3, -8, -8, -8, -1, 5, 1, 1, -1, 6, 5, 1, -1, 5, -8, 8, -7, -5, -1, -1, 6, -8, -3, -1, -2, -6, -5, -5, -6, 0, 2, 2, 7, -1, -5, -7, -1, -3, 7, 6, 0, 2, 4, -5, 0, -4]
  ]
