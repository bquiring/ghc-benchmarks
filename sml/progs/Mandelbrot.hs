-- mandelbrot.sml (3cps test/programs/llvm-sml; John Reppy, from the SML/NJ
-- benchmark suite): total iteration count over a 1024x1024 grid.
module Main (main) where

import Repeat

compute :: Int -> Int -> Int
compute sz maxCount = loop1 0 0
  where
    x0, y0, wid, delta :: Double
    x0 = -2.0
    y0 = 1.25
    wid = 2.5
    delta = wid / fromIntegral sz
    loop1 :: Int -> Int -> Int
    loop1 i totCount
      | i >= sz   = totCount
      | otherwise = loop1 (i + 1) (loop2 0 totCount)
      where
        c_im = y0 - delta * fromIntegral i
        loop2 :: Int -> Int -> Int
        loop2 j totCount'
          | j >= sz   = totCount'
          | otherwise = loop2 (j + 1) (totCount' + loop3 0 c_re c_im)
          where
            c_re = x0 * (delta + fromIntegral j)
            loop3 :: Int -> Double -> Double -> Int
            loop3 n z_re z_im
              | n < maxCount =
                  let z_re_sq = z_re * z_re
                      z_im_sq = z_im * z_im
                  in if z_re_sq + z_im_sq > 4.0
                       then n
                       else let z_re_im = z_re * z_im
                            in loop3 (n + 1) (z_re_sq - z_im_sq + c_re) (z_re_im + z_re_im + c_im)
              | otherwise = n

main :: IO ()
main = do
  n <- iterations 20
  let results = [ compute (vary i 1024 0) (8 * 1024) | i <- [1 .. n] ]
  case filter (/= 4274066) results of
    []      -> putStrLn "pass"
    (r : _) -> putStrLn ("fail with " ++ show r)
