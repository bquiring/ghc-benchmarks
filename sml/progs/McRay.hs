-- mc-ray.sml (3cps test/programs/llvm-sml; John Reppy, an SML port of the
-- "Weekend Raytracer" by Peter Shirley): a Monte Carlo ray tracer over a
-- random scene of spheres (objects and materials are closures), rendering
-- a 5x5 image with 100 samples per pixel, repeated.
--
-- The SML draws random numbers from a global Park-Miller generator (a ref);
-- here it is an IORef and the code that draws runs in IO, drawing in the
-- SML's evaluation order (left to right).  Each round restarts the
-- generator, so every round renders the same image.
module Main (main) where

import Control.Monad (foldM, forM)
import Data.IORef
import Repeat

-- Park-Miller (MINSTD)
newtype Rand = Rand (IORef Int)

gen32 :: Rand -> IO Int
gen32 (Rand state) = do
  x <- readIORef state
  let x' = (x * 48271) `mod` 2147483647
  writeIORef state x'
  return x'

rand :: Rand -> IO Double
rand r = (\x -> (1.0 / 2147483647.0) * fromIntegral x) <$> gen32 r

type Interval = (Double, Double)

within :: Double -> Interval -> Bool
within t (mn, mx) = mn <= t && t <= mx

type RGB = (Double, Double, Double)

rgbAdd, rgbModulate :: RGB -> RGB -> RGB
rgbAdd (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)
rgbModulate (r1, g1, b1) (r2, g2, b2) = (r1 * r2, g1 * g2, b1 * b2)

rgbScale :: Double -> RGB -> RGB
rgbScale s (r, g, b) = (s * r, s * g, s * b)

rgbLerp :: RGB -> Double -> RGB -> RGB
rgbLerp c1 t c2 = rgbAdd (rgbScale (1.0 - t) c1) (rgbScale t c2)

black, white :: RGB
black = (0.0, 0.0, 0.0)
white = (1.0, 1.0, 1.0)

gray :: Double -> RGB
gray v = (v, v, v)

type Color = (Int, Int, Int)

toByte :: Double -> Int
toByte f = let f' = floor (f * 255.99) in if f' <= 0 then 0 else if 255 <= f' then 255 else f'

fromRGBWithGamma :: RGB -> Color
fromRGBWithGamma (r, g, b) = (cvt r, cvt g, cvt b)
  where cvt x = toByte (sqrt x)

type Vec3 = (Double, Double, Double)

vEpsilon :: Double
vEpsilon = 0.0001

vZero :: Vec3
vZero = (0.0, 0.0, 0.0)

vAdd, vSub, vCross :: Vec3 -> Vec3 -> Vec3
vAdd (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
vSub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
vCross (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

vScale :: Double -> Vec3 -> Vec3
vScale s (x, y, z) = (s * x, s * y, s * z)

vAdds :: Vec3 -> Double -> Vec3 -> Vec3
vAdds v1 s v2 = vAdd v1 (vScale s v2)

vDot :: Vec3 -> Vec3 -> Double
vDot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

vLength :: Vec3 -> Double
vLength v = sqrt (vDot v v)

vNormalize :: Vec3 -> Vec3
vNormalize v = let l = vLength v in if l < vEpsilon then vZero else vScale (1.0 / l) v

vReflect :: Vec3 -> Vec3 -> Vec3
vReflect v n = vAdds v (-2.0 * vDot v n) n

randomPointInSphere :: Rand -> IO Vec3
randomPointInSphere r = do
  x <- rand r
  y <- rand r
  z <- rand r
  let pt = (x, y, z)
  if vDot pt pt < 1.0 then return pt else randomPointInSphere r

type Ray = (Vec3, Vec3)

rayMake :: Vec3 -> Vec3 -> Ray
rayMake origin dir = (origin, vNormalize dir)

rayEval :: Ray -> Double -> Vec3
rayEval (origin, dir) t = vAdds origin t dir

-- Materials: an emission function and a (random) scattering function
data Hit = Hit Double Vec3 Vec3 Material
data Material = Material (Hit -> RGB) (Ray -> Hit -> IO (Maybe (RGB, Ray)))

getEmission :: Hit -> RGB
getEmission hit@(Hit _ _ _ (Material emit _)) = emit hit

getHitInfo :: Hit -> Ray -> IO (Maybe (RGB, Ray))
getHitInfo hit@(Hit _ _ _ (Material _ scatter)) ray = scatter ray hit

lambertian :: Rand -> RGB -> Material
lambertian r albedo = Material (\_ -> black) (\_ (Hit _ pt norm _) -> do
  p <- randomPointInSphere r
  return (Just (albedo, rayMake pt (vAdd norm p))))

metal :: Rand -> RGB -> Double -> Material
metal r albedo fuzz = Material (\_ -> black) (\(_, rdir) (Hit _ pt norm _) -> do
  p <- randomPointInSphere r
  let dir = vAdds (vReflect rdir norm) fuzz p
  return (if vDot dir norm <= 0.0 then Nothing else Just (albedo, rayMake pt dir)))

-- Objects: hit tests
data MaybeHit = Miss | MHit Hit
newtype Object = Obj (Ray -> Interval -> MaybeHit)

hitTest :: Object -> Ray -> Interval -> MaybeHit
hitTest (Obj hit) ray minMaxT = hit ray minMaxT

closer :: MaybeHit -> MaybeHit -> MaybeHit
closer Miss maybeHit = maybeHit
closer maybeHit Miss = maybeHit
closer hit1@(MHit (Hit t1 _ _ _)) hit2@(MHit (Hit t2 _ _ _)) = if t1 <= t2 then hit1 else hit2

fromList :: [Object] -> Object
fromList [] = Obj (\_ _ -> Miss)
fromList [obj] = obj
fromList objs = Obj (\ray minMaxT -> foldl (\mhit obj -> closer mhit (hitTest obj ray minMaxT)) Miss objs)

sphere :: Vec3 -> Double -> Material -> Object
sphere center radius material = Obj hit
  where
    rSq = radius * radius
    invR = 1.0 / radius
    hit ray@(ro, rd) minMaxT =
      let q = vSub ro center
          b = 2.0 * vDot rd q
          c = vDot q q - rSq
          disc = b * b - 4.0 * c
      in if disc < 0.0 then Miss
         else let t = 0.5 * (negate b - sqrt disc)
              in if within t minMaxT
                   then let pt = rayEval ray t
                        in MHit (Hit t pt (vScale invR (vSub pt center)) material)
                   else Miss

data Image = Img Int Int [Color]

data Camera = Cam Int Int Int Vec3 Vec3 Vec3 Vec3

cameraMake :: Int -> Int -> Int -> Vec3 -> Vec3 -> Vec3 -> Double -> Camera
cameraMake wid ht ns pos lookAt up0 fov =
  let dir = vNormalize (vSub lookAt pos)
      right = vNormalize (vCross dir up0)
      up = vNormalize (vCross right dir)
      pw = 2.0 / fromIntegral wid
      aspect = fromIntegral ht / fromIntegral wid
      theta = (pi * fov) / 180.0
      flen = 1.0 / tan (0.5 * theta)
      imgCenter = vAdd pos (vScale flen dir)
      ulc = vSub (vAdd imgCenter (vScale aspect up)) right
  in Cam wid ht ns pos ulc (vScale pw right) (vScale (negate pw) up)

-- Pixels are rendered from the last row and column backwards, as in the SML
foreachPixel :: Camera -> (Int -> Int -> IO Color) -> IO Image
foreachPixel (Cam wid ht _ _ _ _ _) pr = Img wid ht <$> rowLp (ht - 1) []
  where rowLp r colors = if r < 0 then return colors else colLp (r - 1) (wid - 1) colors
        colLp r c colors = if c < 0 then rowLp r colors
                           else pr r c >>= \col -> colLp r (c - 1) (col : colors)

rayToRGB :: Ray -> RGB
rayToRGB (_, (_, y, _)) = rgbLerp white (0.5 * (y + 1.0)) (0.5, 0.7, 1.0)

raysForPixel :: Rand -> Camera -> Int -> Int -> IO [Ray]
raysForPixel rnd (Cam _ _ ns pos ulc hvec vvec) r c = forM [0 .. ns - 1] mkRay
  where ulcDir = vSub ulc pos
        mkRay _ = do
          a <- rand rnd
          let dir = vAdds ulcDir (fromIntegral r + a) vvec
          b <- rand rnd
          let dir' = vAdds dir (fromIntegral c + b) hvec
          return (rayMake pos dir')

aaPixelToRGB :: Rand -> Camera -> (Ray -> IO RGB) -> Int -> Int -> IO RGB
aaPixelToRGB rnd cam@(Cam _ _ ns _ _ _ _) trace r c = do
  rays <- raysForPixel rnd cam r c
  let scale = if ns == 0 then 1.0 else 1.0 / fromIntegral ns
  rgbScale scale <$> foldM (\col ray -> rgbAdd col <$> trace ray) black rays

traceRay :: Object -> Int -> Ray -> IO RGB
traceRay world maxDepth ray0 = trace ray0 maxDepth
  where
    minMaxT = (0.001, 1 / 0)
    trace ray depth
      | depth <= 0 = return black
      | otherwise = case hitTest world ray minMaxT of
          Miss -> return (rayToRGB ray)
          MHit hit -> getHitInfo hit ray >>= \info -> case info of
            Nothing -> return (getEmission hit)
            Just (aten, reflect) -> rgbAdd (getEmission hit) . rgbModulate aten <$> trace reflect (depth - 1)

rayTracer :: Rand -> Camera -> Object -> IO Image
rayTracer rnd cam world =
  foreachPixel cam (\r c -> fromRGBWithGamma <$> aaPixelToRGB rnd cam (traceRay world 20) r c)

randomSphere :: Rand -> Int -> Int -> IO Object
randomSphere r x z = do
  chooseMat <- rand r
  a <- rand r
  b <- rand r
  let c = (fromIntegral x + 0.9 * a, 0.2, fromIntegral z + 0.9 * b)
  mat <- if chooseMat < 0.8
    then do
      r1 <- rand r; r2 <- rand r; g1 <- rand r; g2 <- rand r; b1 <- rand r; b2 <- rand r
      return (lambertian r (r1 * r2, g1 * g2, b1 * b2))
    else do
      r1 <- rand r; g1 <- rand r; b1 <- rand r; f <- rand r
      return (metal r (0.5 * (1.0 + r1), 0.5 * (1.0 + g1), 0.5 * (1.0 + b1)) (0.5 * f))
  return (sphere c 0.2 mat)

makeScene :: Rand -> IO Object
makeScene r = fromList <$> lp (-11) (-11)
  [ sphere (0.0, -1000.0, 0.0) 1000.0 (lambertian r (gray 0.5))
  , sphere (4.0, 1.0, 0.0) 1.0 (metal r (0.7, 0.6, 0.5) 0.0)
  , sphere (-4.0, 1.0, 0.0) 1.0 (lambertian r (0.4, 0.2, 0.1)) ]
  where lp x z objs
          | z < 11 = randomSphere r x z >>= \s -> lp x (z + 1) (s : objs)
          | x < 11 = lp (x + 1) (-11) objs
          | otherwise = return objs

test :: Int -> Int -> Int -> IO Image
test wid ht ns = do
  r <- Rand <$> newIORef 1234567
  let cam = cameraMake wid ht ns (13.0, 2.0, 3.0) vZero (0.0, 1.0, 0.0) 30.0
  world <- makeScene r
  rayTracer r cam world

resultLst :: [Color]
resultLst =
  [ (215,232,255),(215,232,255),(215,232,255),(215,232,255),(215,232,255)
  , (219,234,255),(219,234,255),(219,234,255),(219,234,255),(219,234,255)
  , (188,203,225),(177,181,195),(175,176,184),(173,183,194),(190,203,220)
  , (107,119,154),(104,121,150),(117,125,139),(109,115,119),(128,135,166)
  , (121,161,162),(125,130,168),(131,144,178),(123,142,157),(119,138,174) ]

main :: IO ()
main = do
  n <- iterations 25
  imgs <- forM [1 .. n] $ \i -> do
    Img w h lst <- test (vary i 5 0) 5 100
    return (w == 5 && h == 5 && lst == resultLst)
  putStrLn (if and imgs then "pass" else "fail")
