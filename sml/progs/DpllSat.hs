-- dpll-sat.sml (3cps test/programs/higher-order): a DPLL SAT solver with
-- success and failure continuations (the failure continuation escapes into
-- a closure for backtracking), and the variable-choice heuristic passed as
-- a function.  Literals get an opaque zero k added each round
-- (Repeat.vary).  SML functions taking tuples are curried.
module Main (main) where

import Repeat

-- Partial assignment: sorted list, var -> bool
mapFind :: [(Int, Bool)] -> Int -> Maybe Bool
mapFind [] _ = Nothing
mapFind ((k', v) : rest) k = if k == k' then Just v else if k > k' then mapFind rest k else Nothing

mapIns :: [(Int, Bool)] -> Int -> Bool -> [(Int, Bool)]
mapIns [] k v = [(k, v)]
mapIns m@((k', v') : rest) k v
  | k == k' = (k, v) : rest
  | k < k' = (k, v) : m
  | otherwise = (k', v') : mapIns rest k v

-- A literal is a signed int: positive true, negative false polarity
litVar :: Int -> Int
litVar x = if x > 0 then x else 0 - x

litSign :: Int -> Bool
litSign x = x > 0

evalLit :: [(Int, Bool)] -> Int -> Maybe Bool
evalLit assign lit = case mapFind assign (litVar lit) of
  Nothing -> Nothing
  Just b -> Just (if litSign lit then b else not b)

-- Drop false literals; Nothing if the clause is satisfied
simpClause :: [(Int, Bool)] -> [Int] -> Maybe [Int]
simpClause assign clause = go clause []
  where go [] acc = Just acc
        go (lit : rest) acc = case evalLit assign lit of
          Just True -> Nothing
          Just False -> go rest acc
          Nothing -> go rest (lit : acc)

-- Drop satisfied clauses, shrink the others; Nothing on a conflict
simpFormula :: [(Int, Bool)] -> [[Int]] -> Maybe [[Int]]
simpFormula assign formula = go formula []
  where go [] acc = Just acc
        go (cl : rest) acc = case simpClause assign cl of
          Nothing -> go rest acc
          Just [] -> Nothing
          Just cl' -> go rest (cl' : acc)

unitProp :: [[Int]] -> [(Int, Bool)] -> Maybe ([[Int]], [(Int, Bool)])
unitProp formula assign = case findUnit formula of
  Nothing -> Just (formula, assign)
  Just lit ->
    let assign' = mapIns assign (litVar lit) (litSign lit)
    in case simpFormula assign' formula of
         Nothing -> Nothing
         Just f' -> unitProp f' assign'
  where findUnit [] = Nothing
        findUnit ([lit] : _) = Just lit
        findUnit (_ : rest) = findUnit rest

chooseFirst :: [[Int]] -> [(Int, Bool)] -> Int
chooseFirst formula assign = case formula of
  [] -> -1
  (lit : _) : _ ->
    let v = litVar lit
    in case mapFind assign v of
         Nothing -> v
         Just _ -> chooseFirst (case formula of { [] -> []; _ : rest -> rest }) assign
  _ : rest -> chooseFirst rest assign

dpll :: ([[Int]] -> [(Int, Bool)] -> Int) -> [[Int]] -> [(Int, Bool)]
     -> ([(Int, Bool)] -> r) -> (() -> r) -> r
dpll choose formula assign sk fk = case simpFormula assign formula of
  Nothing -> fk ()
  Just [] -> sk assign
  Just f -> case unitProp f assign of
    Nothing -> fk ()
    Just (f', assign') -> case simpFormula assign' f' of
      Nothing -> fk ()
      Just [] -> sk assign'
      Just f'' ->
        let v = choose f'' assign'
        in if v < 0 then fk ()
           else dpll choose f'' (mapIns assign' v True) sk
                  (\() -> dpll choose f'' (mapIns assign' v False) sk fk)

verify :: [[Int]] -> [(Int, Bool)] -> Bool
verify formula assign = go formula
  where clauseOk [] = False
        clauseOk (lit : rest) = case evalLit assign lit of
          Just True -> True
          _ -> clauseOk rest
        go [] = True
        go (cl : rest) = clauseOk cl && go rest

solve :: ([[Int]] -> [(Int, Bool)] -> Int) -> [[Int]] -> Maybe [(Int, Bool)]
solve choose formula = dpll choose formula [] Just (\() -> Nothing)

total :: Int -> Int
total k =
  let satFormula = [[1 + k, 2 + k], [-1 - k, 3 + k], [-2 - k, -3 - k]]
      unsatFormula = [[1 + k], [-1 - k]]
      r1 = case solve chooseFirst satFormula of
             Nothing -> 0
             Just assign -> if verify satFormula assign then 1 else 0
      r2 = case solve chooseFirst unsatFormula of
             Nothing -> 1
             Just _ -> 0
  in r1 + r2

main :: IO ()
main = do
  n <- iterations 1647267
  putStrLn (if loopLast n (\i -> total (vary i 0 0)) == 2 then "pass" else "fail")
