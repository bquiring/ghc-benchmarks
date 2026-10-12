-- logic.sml (MLton benchmarks, 3cps test/programs/from-mlton; from the
-- SML/NJ benchmark suite): a peg-solitaire puzzle solved by Prolog clauses
-- compiled to SML: logic variables are refs, a global trail undoes
-- bindings on backtracking, success continuations drive the search, and
-- the first solution escapes with an exception (so the trail is not
-- unwound, and carries over to the next round, as in the SML).
--
-- The clauses (move_horiz .. solution2) are translated mechanically by
-- tools/logic-clauses.py; the rest by hand.  Everything runs in IO; the
-- SML's global refs are top-level IORefs.  Prints testit's "yes".
module Main (main) where

import Control.Exception
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Repeat

data Term = STR String [Term] | INT Int | CON String | REF (IORef (Maybe Term))

str :: (String, [Term]) -> Term
str (f, ts) = STR f ts

newtype BadArg = BadArg String deriving Show
instance Exception BadArg

-- Trail
globalTrail :: IORef [IORef (Maybe Term)]
globalTrail = unsafePerformIO (newIORef [])
{-# NOINLINE globalTrail #-}

trailCounter :: IORef Int
trailCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE trailCounter #-}

unwindTrail :: Int -> [IORef (Maybe Term)] -> IO [IORef (Maybe Term)]
unwindTrail 0 tr = return tr
unwindTrail n (r : tr) = writeIORef r Nothing >> unwindTrail (n - 1) tr
unwindTrail _ [] = throwIO (BadArg "unwind_trail")

trail :: IO () -> IO ()
trail func = do
  tc0 <- readIORef trailCounter
  func
  tc <- readIORef trailCounter
  gt <- readIORef globalTrail
  gt' <- unwindTrail (tc - tc0) gt
  writeIORef globalTrail gt'
  writeIORef trailCounter tc0

bind :: IORef (Maybe Term) -> Term -> IO ()
bind r t = do
  writeIORef r (Just t)
  modifyIORef globalTrail (r :)
  modifyIORef trailCounter (+ 1)

-- Unify
sameRef :: IORef (Maybe Term) -> Term -> Bool
sameRef r (REF r') = r == r'
sameRef _ _ = False

occursCheck :: IORef (Maybe Term) -> Term -> IO Bool
occursCheck r = oc
  where oc (STR _ ts) = ocs ts
        oc (REF r') = readIORef r' >>= \v -> case v of
          Just s -> oc s
          Nothing -> return (r /= r')
        oc (CON _) = return True
        oc (INT _) = return True
        ocs [] = return True
        ocs (t : ts) = oc t >>= \b -> if b then ocs ts else return False

deref :: Term -> IO Term
deref t@(REF x) = readIORef x >>= \v -> case v of
  Just s -> deref s
  Nothing -> return t
deref t = return t

unify' :: (Term, Term) -> IO () -> IO ()
unify' (REF r, t) sc = unifyREF r t sc
unify' (s, REF r) sc = unifyREF r s sc
unify' (STR f ts, STR g ss) sc = if f == g then unifys ts ss sc else return ()
unify' (CON f, CON g) sc = if f == g then sc else return ()
unify' (INT f, INT g) sc = if f == g then sc else return ()
unify' _ _ = return ()

unifys :: [Term] -> [Term] -> IO () -> IO ()
unifys [] [] sc = sc
unifys (t : ts) (s : ss) sc = do
  t' <- deref t
  s' <- deref s
  unify' (t', s') (unifys ts ss sc)
unifys _ _ _ = return ()

unifyREF :: IORef (Maybe Term) -> Term -> IO () -> IO ()
unifyREF r t sc
  | sameRef r t = sc
  | otherwise = occursCheck r t >>= \ok -> if ok then bind r t >> sc else return ()

unify :: (Term, Term) -> IO () -> IO ()
unify (s, t) sc = do
  s' <- deref s
  t' <- deref t
  unify' (s', t') sc

-- Data
cons_s, x_s, nil_s, o_s, s_s :: String
cons_s = "cons"
x_s = "x"
nil_s = "nil"
o_s = "o"
s_s = "s"

con_o_s, con_nil_s, con_x_s :: Term
con_o_s = CON o_s
con_nil_s = CON nil_s
con_x_s = CON x_s

exists :: (Term -> IO ()) -> IO ()
exists sc = newIORef Nothing >>= \r -> sc (REF r)

move_horiz, rotate, move :: (Term, Term) -> IO () -> IO ()
solitaire :: (Term, Term, Term) -> IO () -> IO ()
solution1, solution2 :: Term -> IO () -> IO ()

move_horiz (t_1, t_2) sc =
  (
  trail (
  (
  trail (
  (
  trail (
  (
  trail (
  (
  trail (
  (
  trail (
  (
  trail (
  (
  trail (
  (
  trail (
  (
  trail (
  (
  trail (
  exists (\t ->
  exists (\tt ->
  unify (t_1, str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_o_s, t])])]), tt])) (
  unify (t_2, str(cons_s, [str(cons_s, [con_o_s, str(cons_s, [con_o_s, str(cons_s, [con_x_s, t])])]), tt])) (
  sc)))))
  >>
  exists (\p1 ->
  exists (\p5 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [str(cons_s, [p1, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [p5, con_nil_s])])])])]), tt])) (
  unify (t_2, str(cons_s, [str(cons_s, [p1, str(cons_s, [con_o_s, str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [p5, con_nil_s])])])])]), tt])) (
  sc)))))
  ))
  >>
  exists (\p1 ->
  exists (\p2 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [str(cons_s, [p1, str(cons_s, [p2, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_o_s, con_nil_s])])])])]), tt])) (
  unify (t_2, str(cons_s, [str(cons_s, [p1, str(cons_s, [p2, str(cons_s, [con_o_s, str(cons_s, [con_o_s, str(cons_s, [con_x_s, con_nil_s])])])])]), tt])) (
  sc)))))
  ))
  >>
  exists (\l1 ->
  exists (\p4 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [l1, str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [p4, con_nil_s])])])]), tt])])) (
  unify (t_2, str(cons_s, [l1, str(cons_s, [str(cons_s, [con_o_s, str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [p4, con_nil_s])])])]), tt])])) (
  sc)))))
  ))
  >>
  exists (\l1 ->
  exists (\p1 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [l1, str(cons_s, [str(cons_s, [p1, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_o_s, con_nil_s])])])]), tt])])) (
  unify (t_2, str(cons_s, [l1, str(cons_s, [str(cons_s, [p1, str(cons_s, [con_o_s, str(cons_s, [con_o_s, str(cons_s, [con_x_s, con_nil_s])])])]), tt])])) (
  sc)))))
  ))
  >>
  exists (\l1 ->
  exists (\l2 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [l1, str(cons_s, [l2, str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_o_s, con_nil_s])])]), tt])])])) (
  unify (t_2, str(cons_s, [l1, str(cons_s, [l2, str(cons_s, [str(cons_s, [con_o_s, str(cons_s, [con_o_s, str(cons_s, [con_x_s, con_nil_s])])]), tt])])])) (
  sc)))))
  ))
  >>
  exists (\t ->
  exists (\tt ->
  unify (t_1, str(cons_s, [str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, t])])]), tt])) (
  unify (t_2, str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [con_o_s, t])])]), tt])) (
  sc))))
  ))
  >>
  exists (\p1 ->
  exists (\p5 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [str(cons_s, [p1, str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [p5, con_nil_s])])])])]), tt])) (
  unify (t_2, str(cons_s, [str(cons_s, [p1, str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [con_o_s, str(cons_s, [p5, con_nil_s])])])])]), tt])) (
  sc)))))
  ))
  >>
  exists (\p1 ->
  exists (\p2 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [str(cons_s, [p1, str(cons_s, [p2, str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, con_nil_s])])])])]), tt])) (
  unify (t_2, str(cons_s, [str(cons_s, [p1, str(cons_s, [p2, str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [con_o_s, con_nil_s])])])])]), tt])) (
  sc)))))
  ))
  >>
  exists (\l1 ->
  exists (\p4 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [l1, str(cons_s, [str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [p4, con_nil_s])])])]), tt])])) (
  unify (t_2, str(cons_s, [l1, str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [con_o_s, str(cons_s, [p4, con_nil_s])])])]), tt])])) (
  sc)))))
  ))
  >>
  exists (\l1 ->
  exists (\p1 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [l1, str(cons_s, [str(cons_s, [p1, str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, con_nil_s])])])]), tt])])) (
  unify (t_2, str(cons_s, [l1, str(cons_s, [str(cons_s, [p1, str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [con_o_s, con_nil_s])])])]), tt])])) (
  sc)))))
  ))
  >>
  exists (\l1 ->
  exists (\l2 ->
  exists (\tt ->
  unify (t_1, str(cons_s, [l1, str(cons_s, [l2, str(cons_s, [str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, con_nil_s])])]), tt])])])) (
  unify (t_2, str(cons_s, [l1, str(cons_s, [l2, str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [con_o_s, con_nil_s])])]), tt])])])) (
  sc)))))
  )

rotate (t_1, t_2) sc =
  exists (\p11 ->
  exists (\p12 ->
  exists (\p13 ->
  exists (\p14 ->
  exists (\p15 ->
  exists (\p21 ->
  exists (\p22 ->
  exists (\p23 ->
  exists (\p24 ->
  exists (\p31 ->
  exists (\p32 ->
  exists (\p33 ->
  exists (\p41 ->
  exists (\p42 ->
  exists (\p51 ->
  unify (t_1, str(cons_s, [str(cons_s, [p11, str(cons_s, [p12, str(cons_s, [p13, str(cons_s, [p14, str(cons_s, [p15, con_nil_s])])])])]), str(cons_s, [str(cons_s, [p21, str(cons_s, [p22, str(cons_s, [p23, str(cons_s, [p24, con_nil_s])])])]), str(cons_s, [str(cons_s, [p31, str(cons_s, [p32, str(cons_s, [p33, con_nil_s])])]), str(cons_s, [str(cons_s, [p41, str(cons_s, [p42, con_nil_s])]), str(cons_s, [str(cons_s, [p51, con_nil_s]), con_nil_s])])])])])) (
  unify (t_2, str(cons_s, [str(cons_s, [p51, str(cons_s, [p41, str(cons_s, [p31, str(cons_s, [p21, str(cons_s, [p11, con_nil_s])])])])]), str(cons_s, [str(cons_s, [p42, str(cons_s, [p32, str(cons_s, [p22, str(cons_s, [p12, con_nil_s])])])]), str(cons_s, [str(cons_s, [p33, str(cons_s, [p23, str(cons_s, [p13, con_nil_s])])]), str(cons_s, [str(cons_s, [p24, str(cons_s, [p14, con_nil_s])]), str(cons_s, [str(cons_s, [p15, con_nil_s]), con_nil_s])])])])])) (
  sc)))))))))))))))))

move (t_1, t_2) sc =
  (
  trail (
  (
  trail (
  exists (\x ->
  exists (\y ->
  unify (t_1, x) (
  unify (t_2, y) (
  move_horiz (x, y) sc)))))
  >>
  exists (\x ->
  exists (\x1 ->
  exists (\y ->
  exists (\y1 ->
  unify (t_1, x) (
  unify (t_2, y) (
  rotate (x, x1) (
  move_horiz (x1, y1) (
  rotate (y, y1) sc))))))))
  ))
  >>
  exists (\x ->
  exists (\x1 ->
  exists (\y ->
  exists (\y1 ->
  unify (t_1, x) (
  unify (t_2, y) (
  rotate (x1, x) (
  move_horiz (x1, y1) (
  rotate (y1, y) sc))))))))
  )

solitaire (t_1, t_2, t_3) sc =
  (
  trail (
  exists (\x ->
  unify (t_1, x) (
  unify (t_2, str(cons_s, [x, con_nil_s])) (
  unify (t_3, INT(0)) (
  sc)))))
  >>
  exists (\n ->
  exists (\x ->
  exists (\y ->
  exists (\z ->
  unify (t_1, x) (
  unify (t_2, str(cons_s, [x, z])) (
  unify (t_3, str(s_s, [n])) (
  move (x, y) (
  solitaire (y, z, n) sc))))))))
  )

solution1 (t_1) sc =
  exists (\x ->
  unify (t_1, x) (
  solitaire (str(cons_s, [str(cons_s, [con_o_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, con_nil_s])])])])]), str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s,
   con_nil_s])])])]), str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, con_nil_s])])]), str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, con_nil_s])]), str(cons_s, [str(cons_s, [con_x_s, con_nil_s]), con_nil_s])])])])])
  , x, str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [INT(0)])])])])])])])])])])])])])) sc))

solution2 (t_1) sc =
  exists (\x ->
  unify (t_1, x) (
  solitaire (str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, con_nil_s])])])])]), str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s, str(cons_s, [con_x_s,
   con_nil_s])])])]), str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_o_s, str(cons_s, [con_x_s, con_nil_s])])]), str(cons_s, [str(cons_s, [con_x_s, str(cons_s, [con_x_s, con_nil_s])]), str(cons_s, [str(cons_s, [con_x_s, con_nil_s]), con_nil_s])])])])])
  , x, str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [str(s_s, [INT(0)])])])])])])])])])])])])])) sc))

-- Main
data Done = Done deriving Show
instance Exception Done

-- One round: does the search find a solution (raise Done)?
doit :: IO Bool
doit = (exists (\z -> solution2 z (throwIO Done)) >> return False) `catch` (\Done -> return True)

main :: IO ()
main = do
  n <- iterations 8
  found <- mapM (\i -> vary i doit doit) [1 .. n]
  putStrLn (if and found then "yes" else "no")
