-- raytracer.sml (3cps test/programs/llvm-sml; John Reppy, from the Impala
-- benchmarks, originally in Id): a recursive ray tracer rendering a 200x200
-- image of ten spheres, repeated.  The SML checks only the pixel count;
-- the harness forces every pixel's colour.
module Main (main) where

import Repeat

epsilon, infinity :: Double
epsilon = 1.0e-6
infinity = 1.0e20

type Vec = (Double, Double, Double)

vecadd, vecsub, vecmult, veccross :: Vec -> Vec -> Vec
vecadd (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
vecsub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
vecmult (x1, y1, z1) (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)
veccross (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - y2 * z1, z1 * x2 - z2 * x1, x1 * y2 - x2 * y1)

vecsum :: [Vec] -> Vec
vecsum = foldl (\b a -> vecadd a b) (0.0, 0.0, 0.0)

vecnorm :: Vec -> (Vec, Double)
vecnorm (x, y, z) = let len = sqrt (x * x + y * y + z * z) in ((x / len, y / len, z / len), len)

vecscale :: Vec -> Double -> Vec
vecscale (x, y, z) a = (a * x, a * y, a * z)

vecdot :: Vec -> Vec -> Double
vecdot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

-- Broken for negative vectors, as in the original
zerovector :: Vec -> Bool
zerovector (x, y, z) = x < epsilon && y < epsilon && z < epsilon

data Light = Directional Vec Vec   -- direction, color
           | Point Vec Vec         -- position, color

lightcolor :: Light -> Vec
lightcolor (Directional _ c) = c
lightcolor (Point _ c) = c

data Surfspec = Ambient Vec | Diffuse Vec | Specular Vec | Specpow Double
              | Reflect Double | Transmit Double | Refract Double | Body Vec

ambientsurf, diffusesurf, specularsurf, bodysurf :: [Surfspec] -> Vec
ambientsurf [] = (0.0, 0.0, 0.0)
ambientsurf (Ambient v : _) = v
ambientsurf (_ : ss) = ambientsurf ss
diffusesurf [] = (0.0, 0.0, 0.0)
diffusesurf (Diffuse v : _) = v
diffusesurf (_ : ss) = diffusesurf ss
specularsurf [] = (0.0, 0.0, 0.0)
specularsurf (Specular v : _) = v
specularsurf (_ : ss) = specularsurf ss
bodysurf [] = (1.0, 1.0, 1.0)
bodysurf (Body v : _) = v
bodysurf (_ : ss) = bodysurf ss

specpowsurf, reflectsurf, transmitsurf, refractsurf :: [Surfspec] -> Double
specpowsurf [] = 8.0
specpowsurf (Specpow r : _) = r
specpowsurf (_ : ss) = specpowsurf ss
reflectsurf [] = 0.0
reflectsurf (Reflect r : _) = r
reflectsurf (_ : ss) = reflectsurf ss
transmitsurf [] = 0.0
transmitsurf (Transmit r : _) = r
transmitsurf (_ : ss) = transmitsurf ss
refractsurf [] = 1.0
refractsurf (Refract r : _) = r
refractsurf (_ : ss) = refractsurf ss

data Sphere = Sphere Vec Double [Surfspec]   -- pos, radius, surface type

spheresurf :: Sphere -> [Surfspec]
spheresurf (Sphere _ _ surf) = surf

lookfrom, lookat, vup :: Vec
lookfrom = (2.1, 1.3, 1.7)
lookat = (0.0, 0.0, 0.0)
vup = (0.0, 0.0, 1.0)

fov :: Double
fov = 45.0

s2, s3 :: [Surfspec]
s2 = [Ambient (0.035, 0.0325, 0.025), Diffuse (0.5, 0.45, 0.35), Specular (0.8, 0.8, 0.8), Specpow 3.0, Reflect 0.5]
s3 = [Ambient (0.1, 0.0, 0.0), Diffuse (0.3, 0.0, 0.0), Specular (0.8, 0.4, 0.4), Transmit 0.7]

testspheres :: [Sphere]
testspheres =
  [ Sphere (0.0, 0.0, 0.0) 0.5 s3
  , Sphere (0.272166, 0.272166, 0.544331) 0.166667 s2
  , Sphere (0.643951, 0.172546, 0.0) 0.166667 s2
  , Sphere (0.172546, 0.643951, 0.0) 0.166667 s2
  , Sphere (-0.371785, 0.0996195, 0.544331) 0.166667 s2
  , Sphere (-0.471405, 0.471405, 0.0) 0.166667 s2
  , Sphere (-0.643951, -0.172546, 0.0) 0.166667 s2
  , Sphere (0.0996195, -0.371785, 0.544331) 0.166667 s2
  , Sphere (-0.172546, -0.643951, 0.0) 0.166667 s2
  , Sphere (0.471405, -0.471405, 0.0) 0.166667 s2 ]

testlights :: [Light]
testlights =
  [ Point (4.0, 3.0, 2.0) (0.288675, 0.288675, 0.288675)
  , Point (1.0, -4.0, 4.0) (0.288675, 0.288675, 0.288675)
  , Point (-3.0, 1.0, 5.0) (0.288675, 0.288675, 0.288675)
  , Directional (-11.0, 1.0, 0.5) (11.0, -1.0, 0.5) ]

background :: Vec
background = (0.078, 0.361, 0.753)

world :: [Sphere]
world = testspheres

-- Assumes the direction vector is normalized
sphereintersect :: Vec -> Vec -> Sphere -> (Bool, Double)
sphereintersect pos dir (Sphere center rad _) =
  let m = vecsub pos center
      m2 = vecdot m m
      bm = vecdot m dir
      disc = bm * bm - m2 + rad * rad
  in if disc < 0.0 then (False, 0.0)
     else let slo = negate bm - sqrt disc
              shi = negate bm + sqrt disc
          in if slo < 0.0
               then if shi < 0.0 then (False, 0.0) else (True, shi)
               else (True, slo)

spherenormal :: Vec -> Sphere -> Vec
spherenormal pos (Sphere spos rad _) = vecscale (vecsub pos spos) (1.0 / rad)

dtor :: Double -> Double
dtor x = x * pi / 180.0

camparams :: Vec -> Vec -> Vec -> Double -> Int -> (Vec, Vec, Vec)
camparams lfrom lat vu fv winsize =
  let initfirstray = vecsub lat lfrom
      (lookdir, dist) = vecnorm initfirstray
      (scrni, _) = vecnorm (veccross lookdir vu)
      (scrnj, _) = vecnorm (veccross scrni lookdir)
      xwinsize = fromIntegral winsize
      ywinsize = fromIntegral winsize
      magx = 2.0 * dist * tan (dtor (fv / 2.0)) / xwinsize
      magy = 2.0 * dist * tan (dtor (fv / 2.0)) / ywinsize
      scrnx = vecscale scrni magx
      scrny = vecscale scrnj magy
      firstray = vecsub initfirstray (vecadd (vecscale scrnx (0.5 * xwinsize)) (vecscale scrny (0.5 * ywinsize)))
  in (firstray, scrnx, scrny)

-- Colour a pixel
tracepixel :: [Sphere] -> [Light] -> Int -> Int -> Vec -> Vec -> Vec -> Vec
tracepixel spheres lights x y firstray scrnx scrny =
  let pos = lookfrom
      (dir, _) = vecnorm (vecadd (vecadd firstray (vecscale scrnx (fromIntegral x))) (vecscale scrny (fromIntegral y)))
      (hit, dist, sp) = trace spheres pos dir
  in if hit then shade lights sp pos dir dist (1.0, 1.0, 1.0) else background

-- The first intersection in the set of all objects
trace :: [Sphere] -> Vec -> Vec -> (Bool, Double, Sphere)
trace spheres pos dir =
  let sphmap [] = []
      sphmap (x : xs) = let (hit, where') = sphereintersect pos dir x
                        in if hit then (where', x) : sphmap xs else sphmap xs
  in case sphmap spheres of
       [] -> (False, infinity, head spheres)   -- missed all
       first : rest ->
         let (mindist, sp) = foldl (\(d2, s2') (d1, s1) -> if d1 < d2 then (d1, s1) else (d2, s2')) first rest
         in (True, mindist, sp)

-- The complete shader
shade :: [Light] -> Sphere -> Vec -> Vec -> Double -> Vec -> Vec
shade lights sp lookpos dir dist contrib =
  let hitpos = vecadd lookpos (vecscale dir dist)
      ambientlight = (1.0, 1.0, 1.0)
      surf = spheresurf sp
      amb = vecmult ambientlight (ambientsurf surf)
      norm = spherenormal hitpos sp
      refl = vecadd dir (vecscale norm ((-2.0) * vecdot dir norm))
      diff = vecsum (map (\l -> lightray l hitpos norm refl surf) lights)
      transmitted = transmitsurf surf
      simple = vecadd amb diff
      trintensity = vecscale (bodysurf surf) transmitted
      (tir, trcol) = if transmitted < epsilon then (False, simple)
                     else transmitray lights simple hitpos dir (refractsurf surf) trintensity contrib norm
      reflsurf = vecscale (specularsurf surf) (reflectsurf surf)
      reflectiv = if tir then vecadd trintensity reflsurf else reflsurf
  in if zerovector reflectiv then trcol
     else reflectray hitpos refl lights reflectiv contrib trcol

-- Transmit a ray through an object
transmitray :: [Light] -> Vec -> Vec -> Vec -> Double -> Vec -> Vec -> Vec -> (Bool, Vec)
transmitray lights color pos dir index intens contrib norm =
  let newcontrib = vecmult intens contrib
  in if zerovector newcontrib then (False, color)
     else let (tir, newdir) = refractray index dir norm
          in if tir then (True, color)
             else let nearpos = vecadd pos (vecscale newdir epsilon)
                      (hit, dist, sp) = trace world nearpos newdir
                      newcol = if hit then shade lights sp nearpos newdir dist newcontrib else background
                  in (False, vecadd (vecmult newcol intens) color)

-- Reflect a ray from an object
reflectray :: Vec -> Vec -> [Light] -> Vec -> Vec -> Vec -> Vec
reflectray pos newdir lights intens contrib color =
  let newcontrib = vecmult intens contrib
  in if zerovector newcontrib then color
     else let nearpos = vecadd pos (vecscale newdir epsilon)
              (hit, dist, sp) = trace world nearpos newdir
              newcol = if hit then shade lights sp nearpos newdir dist newcontrib else background
          in vecadd color (vecmult newcol intens)

-- Refract a ray through a surface: the new direction, and whether total
-- internal reflection occurred
refractray :: Double -> Vec -> Vec -> (Bool, Vec)
refractray newindex olddir innorm =
  let dotp = negate (vecdot olddir innorm)
      (norm, k, nr) = if dotp < 0.0 then (vecscale innorm (-1.0), negate dotp, 1.0 / newindex)
                      else (innorm, dotp, newindex)
      disc = 1.0 - nr * nr * (1.0 - k * k)
  in if disc < 0.0 then (True, (0.0, 0.0, 0.0))
     else let t = nr * k - sqrt disc
          in (False, vecadd (vecscale norm t) (vecscale olddir nr))

-- A light's contribution to the colour of a surface point
lightray :: Light -> Vec -> Vec -> Vec -> [Surfspec] -> Vec
lightray l pos norm refl surf =
  let (ldir, _) = lightdirection l pos
      cosangle = vecdot ldir norm
      (inshadow, lcolor) = shadowed pos ldir (lightcolor l)
  in if inshadow then (0.0, 0.0, 0.0)
     else let diff = diffusesurf surf
              spow = specpowsurf surf
          in if cosangle <= 0.0
               then let bodycol = bodysurf surf
                        cosalpha = negate (vecdot refl ldir)
                        diffcont = vecmult (vecscale diff (negate cosangle)) lcolor
                        speccont = if cosalpha <= 0.0 then (0.0, 0.0, 0.0)
                                   else vecmult (vecscale bodycol (cosalpha ** spow)) lcolor
                    in vecadd diffcont speccont
               else let spec = specularsurf surf
                        cosalpha = vecdot refl ldir
                        diffcont = vecmult (vecscale diff cosangle) lcolor
                        speccont = if cosalpha <= 0.0 then (0.0, 0.0, 0.0)
                                   else vecmult (vecscale spec (cosalpha ** spow)) lcolor
                    in vecadd diffcont speccont

lightdirection :: Light -> Vec -> (Vec, Double)
lightdirection (Directional dir _) _ = let (d, _) = vecnorm dir in (d, infinity)
lightdirection (Point pos _) pt = vecnorm (vecsub pos pt)

shadowed :: Vec -> Vec -> Vec -> (Bool, Vec)
shadowed pos dir lcolor =
  let (hit, _, _) = trace world (vecadd pos (vecscale dir epsilon)) dir
  in if not hit then (False, lcolor) else (True, lcolor)

ray :: Int -> [Vec]
ray winsize =
  let lights = testlights
      (firstray, scrnx, scrny) = camparams lookfrom lookat vup fov winsize
      f i j = tracepixel world lights i j firstray scrnx scrny
      doRow i pxls = if i < 0 then pxls else doCol i (winsize - 1) pxls
      doCol i j pixls = if j < 0 then doRow (i - 1) pixls else doCol i (j - 1) (f i j : pixls)
  in doRow (winsize - 1) []

main :: IO ()
main = do
  n <- iterations 50
  let img = loopLast n (\i -> ray (vary i 200 0))
  putStrLn (if length img == 40000 then "pass" else "fail")
