-- register-allocator.sml (3cps test/programs/higher-order): a
-- graph-colouring register allocator for a tiny three-address IR, with
-- sorted-list sets and maps and a spill-cost function argument.  The
-- program's registers get an opaque zero k added each round (Repeat.vary).
-- SML functions taking tuples are curried.
module Main (main) where

import Repeat

type Set = [Int]

setMem :: Int -> Set -> Bool
setMem _ [] = False
setMem x (y : ys) = x == y || (x > y && setMem x ys)

setIns :: Int -> Set -> Set
setIns x [] = [x]
setIns x xs@(y : ys) = if x == y then xs else if x < y then x : xs else y : setIns x ys

setDel :: Int -> Set -> Set
setDel _ [] = []
setDel x xs@(y : ys)
  | x < y = xs
  | x > y = y : setDel x ys
  | otherwise = ys

setUnion :: Set -> Set -> Set
setUnion [] ys = ys
setUnion (x : rest) ys = setUnion rest (setIns x ys)

setDiff :: Set -> Set -> Set
setDiff [] _ = []
setDiff (x : rest) ys = if setMem x ys then setDiff rest ys else x : setDiff rest ys

setSize :: Set -> Int
setSize [] = 0
setSize (_ : rest) = 1 + setSize rest

setFoldl :: (Int -> b -> b) -> b -> Set -> b
setFoldl _ acc [] = acc
setFoldl f acc (x : rest) = setFoldl f (f x acc) rest

-- Interference graph: int -> int set
type IG = [(Int, Set)]

igFind :: IG -> Int -> Set
igFind [] _ = []
igFind ((k', v) : rest) k = if k == k' then v else if k > k' then igFind rest k else []

igIns :: IG -> Int -> Set -> IG
igIns [] k v = [(k, v)]
igIns g@((k', v') : rest) k v
  | k == k' = (k, v) : rest
  | k < k' = (k, v) : g
  | otherwise = (k', v') : igIns rest k v

-- Colouring: int -> int
type CM = [(Int, Int)]

cmFind :: CM -> Int -> Maybe Int
cmFind [] _ = Nothing
cmFind ((k', v) : rest) k = if k == k' then Just v else if k > k' then cmFind rest k else Nothing

cmIns :: CM -> Int -> Int -> CM
cmIns [] k v = [(k, v)]
cmIns m@((k', v') : rest) k v
  | k == k' = (k, v) : rest
  | k < k' = (k, v) : m
  | otherwise = (k', v') : cmIns rest k v

data Instr = MOV Int Int | ADD Int Int Int | SUB Int Int Int | MUL Int Int Int | RET Int

defOf, useOf :: Instr -> Set
defOf i = case i of
  MOV d _ -> setIns d []
  ADD d _ _ -> setIns d []
  SUB d _ _ -> setIns d []
  MUL d _ _ -> setIns d []
  RET _ -> []
useOf i = case i of
  MOV _ s -> setIns s []
  ADD _ s1 s2 -> setIns s1 (setIns s2 [])
  SUB _ s1 s2 -> setIns s1 (setIns s2 [])
  MUL _ s1 s2 -> setIns s1 (setIns s2 [])
  RET s -> setIns s []

revList :: [a] -> [a]
revList [] = []
revList (x : rest) = revList rest ++ [x]

addEdge :: IG -> Int -> Int -> IG
addEdge g u v
  | u == v = g
  | otherwise = igIns (igIns g u (setIns v (igFind g u))) v (setIns u (igFind g v))

buildIG :: [Instr] -> Set -> IG
buildIG instrs liveOut = go (revList instrs) liveOut []
  where
    foldEdges liveSet g = setFoldl (\u g' -> setFoldl (\v g'' -> addEdge g'' u v) g' liveSet) g liveSet
    go [] _ g = g
    go (i : rest) live g =
      let live' = setUnion (useOf i) (setDiff live (defOf i))
          atDef = setUnion (defOf i) live
          g' = foldEdges atDef g
      in go rest live' g'

igDegree :: IG -> Int -> Int
igDegree g v = setSize (igFind g v)

removeNode :: IG -> Int -> IG
removeNode g v =
  let nbrs = igFind g v
      remU g0 u = igIns g0 u (setDel v (igFind g0 u))
      g' = setFoldl (\u g'' -> remU g'' u) g nbrs
  in igIns g' v []

listDel :: Int -> [Int] -> [Int]
listDel _ [] = []
listDel v (x : rest) = if x < v || x > v then x : listDel v rest else rest

simplify :: IG -> [Int] -> Int -> (Int -> Int) -> (IG, [Int], [Int])
simplify g nodes k spillCost = case findLow nodes of
  Nothing -> (g, [], nodes)
  Just v ->
    let g' = removeNode g v
        nodes' = listDel v nodes
        (g'', stk, spills) = simplify g' nodes' k spillCost
    in (g'', v : stk, spills)
  where findLow [] = Nothing
        findLow (v : rest) = if igDegree g v < k then Just v else findLow rest

selectColor :: Set -> Int -> CM -> Int
selectColor nbrs k coloring = findColor 0
  where used = setFoldl (\u cs -> case cmFind coloring u of { Nothing -> cs; Just c -> setIns c cs }) [] nbrs
        findColor c
          | c >= k = 0 - 1
          | setMem c used = findColor (c + 1)
          | otherwise = c

colorGraph :: IG -> [Int] -> Int -> (Int -> Int) -> CM
colorGraph g nodes k spillCost = select stk []
  where (_, stk, _) = simplify g nodes k spillCost
        select [] coloring = coloring
        select (v : rest) coloring = select rest (cmIns coloring v (selectColor (igFind g v) k coloring))

listNodes :: [Instr] -> Set
listNodes instrs = collect instrs []
  where collect [] s = s
        collect (i : rest) s = collect rest (setUnion (defOf i) (setUnion (useOf i) s))

spillCost :: Int -> Int
spillCost v = v

result :: Int -> (Int, Int)
result k =
  let prog = [ADD (0 + k) (1 + k) (2 + k), MUL (3 + k) (0 + k) (1 + k), SUB (4 + k) (3 + k) (2 + k), RET (4 + k)]
      nodes = listNodes prog
      ig = buildIG prog []
      coloring = colorGraph ig nodes 3 spillCost
      checksum = setFoldl (\v acc -> case cmFind coloring v of { Nothing -> acc; Just c -> acc + v * (c + 1) }) 0 nodes
      numColors = setSize (setFoldl (\v cs -> case cmFind coloring v of { Nothing -> cs; Just c -> setIns c cs }) [] nodes)
  in (checksum, numColors)

main :: IO ()
main = do
  n <- iterations 458375
  let (checksum, numColors) = loopLast n (\i -> result (vary i 0 0))
  putStrLn (if numColors <= 3 && numColors >= 1 && checksum > 0 then "pass" else "fail")
