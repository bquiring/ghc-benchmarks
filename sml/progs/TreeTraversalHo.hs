-- tree-traversal-ho.sml (3cps test/programs/higher-order): pre- and
-- post-order walks of a rose tree driven by visitor functions, a CPS walk,
-- and a search that records its result in a ref (an STRef here).  As in
-- the SML, the expected result is computed once by the same code.  The tree
-- size gets an opaque zero k added each round (Repeat.vary).
module Main (main) where

import Control.Monad.ST
import Data.STRef
import Repeat

data Tree = Nd Int [Tree]

preorder :: ((Int, b) -> b) -> b -> Tree -> b
preorder visit acc (Nd v children) = doKids children (visit (v, acc))
  where doKids [] a = a
        doKids (c : rest) a = doKids rest (preorder visit a c)

postorder :: ((Int, b) -> b) -> b -> Tree -> b
postorder visit acc (Nd v children) = visit (v, doKids children acc)
  where doKids [] a = a
        doKids (c : rest) a = doKids rest (postorder visit a c)

sumCont :: Tree -> (Int -> r) -> r
sumCont (Nd v children) k = goKids children v
  where goKids [] acc = k acc
        goKids (c : rest) acc = sumCont c (\cv -> goKids rest (acc + cv))

findFirst :: (Int -> Bool) -> Tree -> Maybe Int
findFirst p t = runST $ do
  cell <- newSTRef Nothing
  let search (Nd v children) = readSTRef cell >>= \c -> case c of
        Just _ -> return ()
        Nothing -> if p v then writeSTRef cell (Just v) else mapM_ search children
  search t
  readSTRef cell

buildTree :: Int -> Tree
buildTree 0 = Nd 0 []
buildTree 1 = Nd 1 [Nd 0 [], Nd 0 []]
buildTree n = Nd n [buildTree (n - 1), buildTree (n - 2)]

-- The visitors take a pair, as in the SML (they are values passed to the
-- traversals, and the traversals' visit (v, acc) builds the pair)
total :: Int -> Int
total k =
  let t = buildTree (4 + k)
      sumVisit (v, acc) = acc + v
      chkVisit (v, acc) = acc * 3 + v
      r1 = preorder sumVisit 0 t
      r2 = postorder sumVisit 0 t
      r3 = preorder chkVisit 0 t
      r4 = postorder chkVisit 0 t
      r5 = sumCont t (\x -> x)
      r6 = case findFirst (\v -> v >= 3) t of { Just v -> v; Nothing -> -1 }
  in r1 + r2 + r3 + r4 + r5 + r6

main :: IO ()
main = do
  let expected = total 0
  n <- iterations 1938220
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == expected then "pass" else "fail")
