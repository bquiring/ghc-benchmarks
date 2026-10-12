{-# LANGUAGE ScopedTypeVariables #-}
-- zebra.sml (MLton benchmarks, 3cps test/programs/from-mlton; Stephen
-- Weeks): solves the zebra puzzle by exploring 3342 partial assignments,
-- with refs and exceptions for control flow (Done ends the search keeping
-- its state, Inconsistent abandons a branch, Continue carries a closure to
-- resume with).  Runs in IO with Haskell exceptions.  Prints the solution
-- table (the SML's commented-out display) once; the SML checks the count
-- of assignments explored (3342), as does this.
module Main (main) where

import Control.Exception
import Control.Monad (forM_, when)
import Data.IORef
import Data.List (find)
import Repeat

data Cigarette = Blend | BlueMaster | Dunhill | PallMall | Prince deriving (Eq, Show)
data Color = Blue | Green | Red | White | Yellow deriving (Eq, Show)
data Drink = Beer | Coffee | Milk | Tea | Water deriving (Eq, Show)
data Nationality = Dane | English | German | Norwegian | Swede deriving (Eq, Show)
data Pet = Bird | Cat | Dog | Horse | Zebra deriving (Eq, Show)

type Pos = Int

data Attr a = Attr { poss :: [Pos], unknown :: [a], known :: [(Pos, a)] }

data Done = Done deriving Show
instance Exception Done

data Inconsistent = Inconsistent deriving Show
instance Exception Inconsistent

newtype Continue = Continue (IO ())
instance Show Continue where show _ = "Continue"
instance Exception Continue

-- Set r to x while running f; restore it afterwards, except when f raises
-- Done (the search keeps the state it found)
fluidLet :: IORef a -> a -> IO b -> IO b
fluidLet r x f = do
  old <- readIORef r
  writeIORef r x
  res <- try f
  case res of
    Right v -> writeIORef r old >> return v
    Left e -> case fromException e of
      Just Done -> throwIO Done
      Nothing -> writeIORef r old >> throwIO e

-- Call f on each element and the list of the others
tryEach :: [a] -> (a -> [a] -> IO ()) -> IO ()
tryEach l0 f = loop l0 []
  where loop [] _ = return ()
        loop (x : l) ac = f x (l ++ ac) >> loop l (x : ac)

data Search = Search
  { cigarettes :: IORef (Attr Cigarette)
  , colors :: IORef (Attr Color)
  , drinks :: IORef (Attr Drink)
  , nationalities :: IORef (Attr Nationality)
  , pets :: IORef (Attr Pet)
  , num :: IORef Int }

findPos :: Eq a => IORef (Attr a) -> a -> IO (Maybe Pos)
findPos r x = (fmap fst . find (\(_, y) -> x == y) . known) <$> readIORef r

isConsistent :: Search -> IO Bool
isConsistent s = do
  modifyIORef' (num s) (+ 1)
  let smoke = findPos (cigarettes s)
      color = findPos (colors s)
      drink = findPos (drinks s)
      nat = findPos (nationalities s)
      pet = findPos (pets s)
      make f a b = (\x y -> case (x, y) of { (Just x', Just y') -> f x' y'; _ -> True }) <$> a <*> b
      same = make (==)
      adjacent = make (\x y -> x == y - 1 || y == x - 1)
      leftOf = make (\x y -> x == y - 1)
      andM [] = return True
      andM (m : ms) = m >>= \b -> if b then andM ms else return False
  andM [ same (nat English) (color Red)
       , same (nat Swede) (pet Dog)
       , same (nat Dane) (drink Tea)
       , leftOf (color Green) (color White)
       , same (color Green) (drink Coffee)
       , same (smoke PallMall) (pet Bird)
       , same (color Yellow) (smoke Dunhill)
       , same (return (Just 3)) (drink Milk)
       , same (nat Norwegian) (return (Just 1))
       , adjacent (smoke Blend) (pet Cat)
       , adjacent (pet Horse) (smoke Dunhill)
       , same (drink Beer) (smoke BlueMaster)
       , same (nat German) (smoke Prince)
       , adjacent (nat Norwegian) (color Blue)
       , adjacent (drink Water) (smoke Blend) ]

tryAttr :: Search -> IORef (Attr a) -> IO (Attr a -> IO (), IO ()) -> IO ()
tryAttr s r f = do
  Attr ps unk kn <- readIORef r
  case unk of
    [] -> return ()
    _ -> tryEach unk $ \x unk' -> do
      (each, done) <- f
      tryEach ps $ \p ps' -> do
        let attr = Attr { known = (p, x) : kn, unknown = unk', poss = ps' }
        fluidLet r attr (isConsistent s >>= \ok -> when ok (each attr))
      done

data Attrs a = None | One a | Many

loop :: Search -> IO ()
loop s = body `catch` handler
  where
    test :: forall a. IORef (Attr a) -> IO ()
    test r = tryAttr s r $ do
      attrs <- newIORef (None :: Attrs (Attr a))
      let each a = modifyIORef attrs (\c -> case c of { None -> One a; One _ -> Many; Many -> Many })
          done = readIORef attrs >>= \c -> case c of
            None -> throwIO Inconsistent
            One a -> throwIO (Continue (fluidLet r a (loop s)))
            Many -> return ()
      return (each, done)
    explore :: IORef (Attr a) -> IO ()
    explore r = tryAttr s r (return (\_ -> loop s, throwIO Inconsistent))
    body = do
      test (cigarettes s); test (colors s); test (drinks s); test (nationalities s); test (pets s)
      explore (cigarettes s); explore (colors s); explore (drinks s); explore (nationalities s); explore (pets s)
      throwIO Done
    handler e = case fromException e of
      Just Inconsistent -> return ()
      Nothing -> case fromException e of
        Just (Continue f) -> f
        Nothing -> throwIO e

display :: Search -> IO [String]
display s = sequence
  [ line (cigarettes s), line (colors s), line (drinks s), line (nationalities s), line (pets s) ]
  where
    line :: Show a => IORef (Attr a) -> IO String
    line r = do
      kn <- known <$> readIORef r
      return (concat [ let str = maybe "?" (show . snd) (find (\(j, _) -> i == j) kn)
                       in str ++ replicate (12 - length str) ' '
                     | i <- [1 .. 5 :: Int] ])

-- One search: the solution table, and whether 3342 assignments were explored
search :: Int -> IO ([String], Bool)
search k = do
  let ps = [1 + k, 2 + k, 3 + k, 4 + k, 5 + k]
      ini xs = newIORef (Attr ps xs [])
  s <- Search <$> ini [Blend, BlueMaster, Dunhill, PallMall, Prince]
              <*> ini [Blue, Green, Red, White, Yellow]
              <*> ini [Beer, Coffee, Milk, Tea, Water]
              <*> ini [Dane, English, German, Norwegian, Swede]
              <*> ini [Bird, Cat, Dog, Horse, Zebra]
              <*> newIORef 0
  loop s `catch` (\Done -> return ())
  n <- readIORef (num s)
  table <- display s
  return (table, n == 3342)

main :: IO ()
main = do
  n <- iterations 1200
  results <- mapM (\i -> search (vary i 0 0)) [1 .. n]
  let (table, _) = head results
  if all snd results && all ((== table) . fst) results
    then forM_ table putStrLn
    else putStrLn "fail"
