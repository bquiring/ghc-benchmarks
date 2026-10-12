-- tardis.sml (3cps test/programs/llvm-sml; translated to SML by Skye Soss
-- from the Haskell tardis package): scoring a bowling game with a monad
-- that has forwards- and backwards-travelling state, 10000 times.
--
-- The SML monad is impure: bind and fix tie their knots through refs, and
-- the states hold thunks (unit -> int).  Here the monad runs in ST, refs
-- are STRefs, and each thunk is an ST action, run where the SML calls it.
module Main (main) where

import Control.Monad.ST
import Data.STRef
import Repeat

type Fw s = [ST s Int]
type Bw s = ST s (Int, Int)
type T s a = (Bw s, Fw s) -> ST s (a, Bw s, Fw s)

run :: T s a -> Bw s -> Fw s -> ST s (a, Bw s, Fw s)
run m bw fw = m (bw, fw)

pure' :: a -> T s a
pure' a (bw, fw) = return (a, bw, fw)

getPast :: T s (Fw s)
getPast (bw, fw) = return (fw, bw, fw)

getFuture :: T s (Bw s)
getFuture (bw, fw) = return (bw, bw, fw)

sendPast :: Bw s -> T s ()
sendPast bw (_, fw) = return ((), bw, fw)

sendFuture :: Fw s -> T s ()
sendFuture fw (bw, _) = return ((), bw, fw)

bind :: T s a -> (a -> T s b) -> T s b
bind m f (bw, fw) = do
  r <- newSTRef Nothing
  let getBw' = readSTRef r >>= maybe (error "Tardis.bind") id
  (x, bw'', fw') <- m (getBw', fw)
  (x', bw', fw'') <- f x (bw, fw')
  writeSTRef r (Just bw')
  return (x', bw'', fw'')

fix :: ((() -> ST s b) -> T s (() -> ST s b)) -> T s (() -> ST s b)
fix f (bw, fw) = do
  r <- newSTRef Nothing
  let get a = readSTRef r >>= maybe (error "Tardis.fix") ($ a)
  (x, bw', fw') <- f get (bw, fw)
  writeSTRef r (Just x)
  return (x, bw', fw')

data Frame = Strike | Spare !Int | Frame !Int !Int
data LFrame = LStrike !Int !Int | LSpare !Int !Int | LFrame !Int !Int
data Game = Game [Frame] LFrame

lframeScore :: LFrame -> Int
lframeScore (LStrike b1 b2) = 10 + b1 + b2
lframeScore (LSpare b1 _) = 10 + b1
lframeScore (LFrame t1 t2) = t1 + t2

initialFw :: Fw s
initialFw = [return 0]

initialBw :: LFrame -> Bw s
initialBw (LStrike b1 _) = return (10, b1)
initialBw (LSpare _ t1) = return (t1, 10 - t1)
initialBw (LFrame t1 t2) = return (t1, t2)

score :: Game -> ST s [Int]
score (Game frames lastFrame) = do
  (scores, _, _) <- run (go frames) (initialBw lastFrame) initialFw
  mapM id scores
  where
    finalFrameScore = lframeScore lastFrame
    go :: [Frame] -> T s (Fw s)
    go [] = bind getPast (\ss -> case ss of
                             [] -> error "empty"
                             sc : _ -> pure' (((\v -> finalFrameScore + v) <$> sc) : ss))
    go (f : fs) = bind (fix step) (\_ -> go fs)
      where
        step p =
          let score' = p () >>= fst
              throws' = snd <$> p ()
          in bind (sendPast throws') (\() ->
             bind getPast (\ss -> case ss of
               [] -> error "empty"
               sc : _ ->
                 bind (sendFuture (score' : ss)) (\() ->
                 bind getFuture (\nextThrows ->
                 case f of
                   Strike -> pure' (\() -> do
                     (nt, _) <- nextThrows
                     return ( do { v <- sc; (a, _) <- nextThrows; (_, b) <- nextThrows; return (v + 10 + a + b) }
                            , (10, nt) ))
                   Spare firstThrow -> pure' (\() ->
                     return ( do { v <- sc; (a, _) <- nextThrows; return (v + 10 + a) }
                            , (firstThrow, 10 - firstThrow) ))
                   Frame firstThrow secondThrow -> pure' (\() ->
                     return ( (\v -> v + firstThrow + secondThrow) <$> sc
                            , (firstThrow, secondThrow) ))))))

sampleGame :: Game
sampleGame = Game [Strike, Spare 9, Strike, Strike, Strike, Frame 8 1, Spare 7, Strike, Strike] (LStrike 10 10)

main :: IO ()
main = do
  n <- iterations 300000
  let actual = loopLast n (\i -> runST (score (vary i sampleGame sampleGame)))
  putStrLn (if actual == [236, 206, 176, 146, 126, 117, 98, 70, 40, 20, 0] then "pass" else "fail")
