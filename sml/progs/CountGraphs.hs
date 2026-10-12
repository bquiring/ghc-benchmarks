{-# LANGUAGE ScopedTypeVariables #-}
-- count-graphs.sml (MLton benchmarks, 3cps test/programs/from-mlton):
-- counts the triangle-free graphs with 3V - 4 = 2E on up to 11 vertices,
-- by folds over subsets, permutations and graphs that cut the search
-- short with exceptions raised from their callbacks, on mutable arrays.
-- Runs in IO.  Each of the SML's generative local exceptions (a fresh one
-- per call, carrying a value) is a fresh Unique tag with a Dynamic payload,
-- and a handler catches only its own tag.  The SML's print is a no-op; this
-- prints f n for n = 0..11 (as tools/mlton/count-graphs.sml does).
module Main (main) where

import Control.Exception
import Control.Monad (forM_, when)
import Data.Array.IO
import Data.Dynamic
import Data.IORef
import Data.Unique
import Repeat

-- A generative exception: its tag, and the value it carries
data Gen = Gen Unique Dynamic
instance Show Gen where show _ = "Gen"
instance Exception Gen

-- A fresh exception: a raise function, and a handler for it alone
newGen :: Typeable a => IO (a -> IO b, IO a -> (a -> IO a) -> IO a)
newGen = do
  u <- newUnique
  let raise v = throwIO (Gen u (toDyn v))
      handle' body h = body `catch` \e@(Gen u' d) ->
        if u' == u then maybe (throwIO e) h (fromDynamic d) else throwIO e
  return (raise, handle')

fold :: [a] -> (a -> s -> s) -> s -> s
fold lst folder state0 = loop lst state0
  where loop [] s = s
        loop (first : rest) s = loop rest (folder first s)

naturalFold :: Int -> (Int -> s -> s) -> s -> s
naturalFold limit folder state0
  | limit < 0 = error "Domain"
  | otherwise = loop 0 state0
  where loop i s = if i == limit then s else loop (i + 1) (folder i s)

naturalAnyM, naturalAllM :: Int -> (Int -> IO Bool) -> IO Bool
naturalAnyM limit ok
  | limit < 0 = error "Domain"
  | otherwise = loop 0
  where loop i = if i == limit then return False else ok i >>= \b -> if b then return True else loop (i + 1)
naturalAllM limit ok
  | limit < 0 = error "Domain"
  | otherwise = loop 0
  where loop i = if i == limit then return True else ok i >>= \b -> if b then loop (i + 1) else return False

-- Fold over all arrangements of bag elements; pFolder may raise accross
foldOverBagPerms :: forall p a. Typeable a
                 => [[Int]] -> (Int -> p -> a -> (a -> IO (p, a)) -> IO (p, a)) -> p -> (p -> a -> IO a) -> a -> IO a
foldOverBagPerms universe pFolder pState0 folder state0 = do
  (accross, handleAccross) <- newGen
  let outer [] pState state = folder pState state
      outer (fbag@(first : fclone) : rest) pState state = inner fbag first fclone rest [] state pState
      outer ([] : _) _ _ = error "Match"
      inner fbag first fclone rest revOut state pState = do
        state' <- handleAccross
                    (do (newPState, s) <- pFolder first pState state accross
                        outer (fold revOut (:) (case fclone of { [] -> rest; _ -> fclone : rest })) newPState s)
                    return
        case rest of
          [] -> return state'
          (sbag@(second : sclone)) : rest' -> inner sbag second sclone rest' (fbag : revOut) state' pState
          [] : _ -> error "Match"
  outer universe pState0 state0

-- Fold over the tree of subsets; eFolder may raise fini
foldOverSubsets :: forall e a. Typeable a
                => [Int] -> (Int -> Bool -> e -> a -> (a -> IO (e, a)) -> IO (e, a)) -> e -> (e -> a -> IO a) -> a -> IO a
foldOverSubsets universe eFolder eState0 folder state0 = do
  (fini, handleFini) <- newGen
  let f first rest eState isinc state =
        handleFini (do (newEState, newState) <- eFolder first isinc eState state fini
                       outer rest newEState newState)
                   return
      outer [] eState state = folder eState state
      outer (first : rest) eState state = f first rest eState True state >>= f first rest eState False
  outer universe eState0 state0

-- Given the classes of [0, size), refine them for vertex size
refine :: Int -> [[Int]] -> (Int -> Int -> IO Bool) -> IO [[Int]]
refine size classes connected = do
  let sizeMatch x = naturalAllM size $ \y -> (==) <$> connected size y <*> connected x (if y == x then size else y)
      merge cls (merged, cs)
        | merged = return (True, reverse cls : cs)
        | otherwise = do
            ok <- sizeMatch (head cls)
            return (if ok then (True, fold cls (:) [size] : cs) else (False, reverse cls : cs))
      split elem (yes, no) = connected elem size >>= \c -> return (if c then (elem : yes, no) else (yes, elem : no))
      subdivide cls state = case cls of
        [_] -> merge cls state
        _ -> do
          parts <- foldM' cls split ([], [])
          case parts of
            ([], no) -> merge no state
            (yes, []) -> merge yes state
            (yes, no) -> merge yes state >>= merge no
  res <- foldM' classes subdivide (False, [])
  return (case res of
            (True, cs) -> reverse cs
            (False, cs) -> fold cs (:) [[size]])
  where foldM' lst g s0 = loop lst s0
          where loop [] s = return s
                loop (x : xs) s = g x s >>= loop xs

data Fini = Fini deriving Show
instance Exception Fini

-- Is the graph minimal?  The number of clones walked through, if it is
minimal :: Int -> [[Int]] -> (Int -> Int -> IO Bool) -> IO (Maybe Int)
minimal nverts classes connected = do
  perm <- newArray (0, max 0 (nverts - 1)) (-1) :: IO (IOUArray Int Int)
  let pFolder new old state accross = loop 0
        where loop v
                | v == old = writeArray perm old new >> return (old + 1, state)
                | otherwise = do
                    a <- connected old v
                    pv <- readArray perm v
                    b <- connected new pv
                    case (a, b) of
                      (True, False) -> accross state
                      (False, True) -> throwIO Fini
                      _ -> loop (v + 1)
  (Just <$> foldOverBagPerms classes pFolder 0 (\_ state -> return (state + 1)) (0 :: Int))
    `catch` \Fini -> return Nothing

-- Fold over the tree of graphs
foldOverGraphs :: forall e b. Typeable b
               => (Int -> Int -> Bool -> e -> b -> (b -> IO (e, b)) -> IO (e, b)) -> e
               -> (Int -> e -> b -> (b -> IO b) -> IO b) -> b -> IO b
foldOverGraphs eFolder eState0 folder state0 = do
  (noextend, handleNoextend) <- newGen
  vertss <- newIORef ([] :: [[Int]])
  let makeVertss limit = [ [0 .. nverts - 1] | nverts <- [0 .. limit - 1] ]
      findVerts size = do
        vs <- readIORef vertss
        when (size >= length vs) (writeIORef vertss (makeVertss (size + 1)))
        (!! size) <$> readIORef vertss
      f size eState state =
        handleNoextend (folder size eState state noextend >>= g (size + 1)) return
      g size state = do
        indices <- findVerts (size - 1)
        foldOverSubsets indices (\to isinc es s acc -> eFolder (size - 1) to isinc es s acc) eState0
                        (\es s -> f size es s) state
  f 0 eState0 state0

-- Check 3V - 4 - 2E >= 0 or V <= 1 for all full subgraphs
okSoFar :: Int -> [Int] -> (Int -> Int -> IO Bool) -> IO Bool
okSoFar _ verts connected = do
  let short lst = case lst of { [] -> True; [_] -> True; _ -> False }
      eFolder elem isinc eState@(ac, picked) _ _ =
        if isinc
          then do
            ac' <- foldIO picked (\p a -> connected elem p >>= \c -> return (if c then a - 2 else a)) (ac + 3)
            return ((ac', elem : picked), ())
          else return (eState, ())
      folder (ac, picked) state =
        if ac >= 0 || short picked then return state else throwIO Fini
  (foldOverSubsets verts eFolder (-4 :: Int, []) folder () >> return True) `catch` \Fini -> return False
  where foldIO lst g s0 = loop lst s0
          where loop [] s = return s
                loop (x : xs) s = g x s >>= loop xs

h :: Int -> (Int -> (Int -> Int -> IO Bool) -> Int -> IO Int) -> Int -> IO Int
h maxSize folder state0 = do
  rows <- mapM (\v -> newArray (0, max 0 (v - 1)) False) [0 .. maxSize - 1] :: IO [IOUArray Int Bool]
  ctab <- newListArray (0, max 0 (maxSize - 1)) rows :: IO (IOArray Int (IOUArray Int Bool))
  classesv <- newArray (0, maxSize) [] :: IO (IOArray Int [[Int]])
  let order from to = if from > to then (from, to) else (to, from)
      connected from to = let (f', t') = order from to in readArray ctab f' >>= \r -> readArray r t'
      update from to value = let (f', t') = order from to in readArray ctab f' >>= \r -> writeArray r t' value
      triangle vnum e = naturalAnyM e (\f' -> (&&) <$> connected vnum f' <*> connected e f')
      eFolder from to isinc _ state accross = do
        tri <- if isinc then triangle from to else return False
        if tri then accross state else update from to isinc >> return ((), state)
      gFolder size _ state accross = do
        when (size /= 0) $ do
          prevClasses <- readArray classesv (size - 1)
          refine (size - 1) prevClasses connected >>= writeArray classesv size
        cls <- readArray classesv size
        m <- minimal size cls connected
        case m of
          Nothing -> accross state
          Just _ -> do
            ok <- okSoFar size [0 .. size - 1] connected
            if ok
              then do
                state' <- folder size connected state
                if size == maxSize then accross state' else return state'
              else accross state
  foldOverGraphs eFolder () gFolder state0

f :: Int -> IO Int
f maxSize = h maxSize (\size connected state -> do
                         fin <- final size connected
                         return (if fin == 0 then state + 1 else state)) 0
  where final size connected = naturalFoldM size (\from ac ->
          naturalFoldM from (\to ac' -> connected from to >>= \c -> return (if c then ac' - 2 else ac')) ac) (3 * size - 4)
        naturalFoldM limit g s0 = loop 0 s0
          where loop i s = if i == limit then return s else g i s >>= loop (i + 1)

main :: IO ()
main = do
  n <- iterations 1
  rs <- mapM (\i -> mapM (\k -> f (vary i k 0)) [0 .. 11]) [1 .. n]
  if all (== head rs) rs then forM_ (head rs) print else putStrLn "fail"
