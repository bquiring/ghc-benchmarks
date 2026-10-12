{-# LANGUAGE Strict #-}
-- parser-comb.sml (3cps test/programs/llvm-sml; John Reppy): a small
-- expression parser built from a stripped-down version of the SML/NJ
-- Library's parser combinators (heavy use of higher-order functions); 8
-- parses per round, 10000 rounds, each printing pass or fail.
--
-- A stream is the remaining input (scanString's substring); a reader
-- (StringCvt.reader) takes one character.
module Main (main) where

import Control.Monad (forM_)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Repeat

type Reader a s = s -> Maybe (a, s)
type Parser a s = Reader Char s -> Reader a s

wrap :: Parser a s -> (a -> b) -> Parser b s
wrap p f getc strm = case p getc strm of
  Nothing -> Nothing
  Just (x, strm') -> Just (f x, strm')

seqWith :: (a -> b -> c) -> Parser a s -> Parser b s -> Parser c s
seqWith f p1 p2 getc strm = case p1 getc strm of
  Just (t1, strm1) -> case p2 getc strm1 of
    Just (t2, strm2) -> Just (f t1 t2, strm2)
    Nothing -> Nothing
  Nothing -> Nothing

pseq :: Parser a s -> Parser b s -> Parser (a, b) s
pseq = seqWith (\a b -> (a, b))

eatChar :: (Char -> Bool) -> Parser Char s
eatChar p getc strm = case getc strm of
  res@(Just (c, _)) -> if p c then res else Nothing
  _ -> Nothing

char :: Char -> Parser Char s
char c = eatChar (\c' -> c == c')

string :: String -> Parser String s
string s getc strm0 = eat s strm0
  where eat [] strm = Just (s, strm)
        eat (c1 : rest) strm = case getc strm of
          Just (c2, strm') -> if c1 == c2 then eat rest strm' else Nothing
          _ -> Nothing

skipBefore :: (Char -> Bool) -> Parser a s -> Parser a s
skipBefore p q getc strm0 = skip' strm0
  where skip' strm = case getc strm of
          Nothing -> Nothing
          Just (c, strm') -> if p c then skip' strm' else q getc strm

orP :: Parser a s -> Parser a s -> Parser a s
orP p1 p2 getc strm = case p1 getc strm of
  Nothing -> p2 getc strm
  res -> res

zeroOrMore :: Parser a s -> Parser [a] s
zeroOrMore p getc strm0 = parse [] strm0
  where p' = p getc
        parse l strm = case p' strm of
          Just (item, strm') -> parse (item : l) strm'
          Nothing -> Just (reverse l, strm)

token :: (Char -> Bool) -> Parser String s
token p getc strm = case zeroOrMore (eatChar p) getc strm of
  Just (res@(_ : _), strm') -> Just (res, strm')
  _ -> Nothing

-- SML's Int.scan StringCvt.DEC: skip white space, an optional sign
-- (+, - or ~), then at least one digit
intScan :: Parser Int s
intScan getc strm0 = sign (skipWs strm0)
  where
    skipWs strm = case getc strm of
      Just (c, strm') | isSpace c -> skipWs strm'
      _ -> strm
    sign strm = case getc strm of
      Just ('~', strm') -> fmap neg (digits strm')
      Just ('-', strm') -> fmap neg (digits strm')
      Just ('+', strm') -> digits strm'
      _ -> digits strm
    neg (v, s) = (negate v, s)
    digits strm = case getc strm of
      Just (c, strm') | isDigit c -> Just (more (digitVal c) strm')
      _ -> Nothing
    more acc strm = case getc strm of
      Just (c, strm') | isDigit c -> more (acc * 10 + digitVal c) strm'
      _ -> (acc, strm)
    digitVal c = fromEnum c - fromEnum '0'

data Exp = VAR String | NUM Int | ADD Exp Exp | LET String Exp Exp

skipWS :: Parser a s -> Parser a s
skipWS = skipBefore isSpace

numParser :: Parser Exp s
numParser = wrap intScan NUM

idParser :: Parser String s
idParser = seqWith (++) (wrap (eatChar isAlpha) (: [])) (token isAlphaNum)

varParser :: Parser Exp s
varParser = wrap idParser VAR

letParser :: Parser Exp s
letParser getc = wrap
  (pseq (string "let") (pseq (skipWS idParser) (pseq (skipWS (char '=')) (pseq expParser
    (pseq (skipWS (string "in")) expParser)))))
  (\(_, (x, (_, (e1, (_, e2))))) -> LET x e1 e2) getc

expParser :: Parser Exp s
expParser getc = wrap
  (skipWS (pseq (orP letParser (orP numParser varParser)) addParser))
  (\(e, es) -> foldl (\b a -> ADD b a) e es) getc

addParser :: Parser [Exp] s
addParser getc = zeroOrMore (skipWS (wrap (pseq (char '+') expParser) snd)) getc

-- StringCvt.scanString
scanString :: Parser a String -> String -> Maybe a
scanString p s = fmap fst (p getc s)
  where getc [] = Nothing
        getc (c : r) = Just (c, r)

doit :: Int -> [String]
doit i =
  let v s = vary i s ""
      r1 = case scanString addParser (v " let x = 1+2 in x + x ") of
             Just [] -> "pass"; Nothing -> "fail"; _ -> error "Match"
      r2 = case scanString addParser (v "+1+1") of
             Just [ADD (NUM 1) (NUM 1)] -> "pass"; _ -> "fail"
      r3 = case scanString varParser (v "x") of
             Nothing -> "pass"; _ -> "fail"
      r4 = case scanString varParser (v "xx") of
             Just (VAR _) -> "pass"; _ -> "fail"
      r5 = case scanString expParser (v "1+1+1") of
             Just (ADD (NUM 1) (ADD (NUM 1) (NUM 1))) -> "pass"; _ -> "fail"
      r6 = case scanString expParser (v " let x = 1+2 in x + x ") of
             Just (VAR _) -> "pass"; _ -> "fail"
      r7 = case scanString expParser (v " let xx = 1+2 in xx + xx ") of
             Just (LET _ (ADD (NUM 1) (NUM 2)) (ADD (VAR _) (VAR _))) -> "pass"; _ -> "fail"
      r8 = case scanString expParser (v " let xx = 1+2 in let      yy3 = xx + \n 38 \t in xx + yy3 + (let x = 1 in xx) ") of
             Just (LET _ (ADD (NUM 1) (NUM 2)) (LET _ (ADD (VAR _) (NUM 38)) (ADD (VAR _) (VAR _)))) -> "pass"
             _ -> "fail"
  in [r1, r2, r3, r4, r5, r6, r7, r8]

main :: IO ()
main = do
  n <- iterations 250000
  forM_ [1 .. n] $ \i -> mapM_ putStrLn (doit i)
