-- json-decode.sml (3cps test/programs/llvm-sml): JSON decoder combinators
-- (a decoder is a function from a JSON value to a result), decoding a list
-- of meeting records, 100001 times.
module Main (main) where

import Control.DeepSeq (NFData (..))
import Data.List (find)
import Repeat

-- A simplified version of JSON values
data Value = OBJECT [(String, Value)]
           | ARRAY [Value]
           | NULL
           | BOOL Bool
           | INT Int
           | STRING String

data Result a = Err String | Ok a

instance NFData a => NFData (Result a) where
  rnf (Err s) = rnf s
  rnf (Ok x) = rnf x

type Decoder a = Value -> Result a

decode :: Decoder a -> Value -> Result a
decode f jv = f jv

int :: Decoder Int
int jv = case jv of { INT n -> Ok n; _ -> Err "not int" }

string :: Decoder String
string v = case v of { STRING s -> Ok s; _ -> Err "not string" }

array :: Decoder a -> Decoder [a]
array elemDecoder v = decoder v
  where decodeList [] elems = Ok (reverse elems)
        decodeList (jv : jvs) elems = case elemDecoder jv of
          Ok e -> decodeList jvs (e : elems)
          Err msg -> Err msg
        decoder (ARRAY elems) = decodeList elems []
        decoder _ = Err "not array"

seqD :: Decoder a -> Decoder (a -> b) -> Decoder b
seqD d1 d2 jv = case d1 jv of
  Ok v -> case d2 jv of
    Ok k -> Ok (k v)
    Err msg -> Err msg
  Err msg -> Err msg

field :: String -> Decoder a -> Decoder a
field key valueDecoder v = d v
  where pred' (l, _) = l == key
        d jv = case jv of
          OBJECT fields -> case find pred' fields of
            Just (_, v') -> decode valueDecoder v'
            _ -> Err "field not found"
          _ -> Err "not object"

reqField :: String -> Decoder a -> Decoder (a -> b) -> Decoder b
reqField key valueDecoder k = seqD (field key valueDecoder) k

findField :: Value -> String -> Maybe Value
findField jv lab = case jv of
  OBJECT fields -> case find (\(l, _) -> l == lab) fields of
    Nothing -> Nothing
    Just (_, v) -> Just v
  _ -> Nothing   -- should be an error, but returning Nothing is simpler

optField :: String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
optField key valueDecoder objDecoder v = decoder v
  where objDecoder' optFld jv = case objDecoder jv of
          Ok d -> Ok (d optFld)
          Err msg -> Err msg
        decoder jv = case findField jv key of
          Just NULL -> objDecoder' Nothing jv
          Just jv' -> case valueDecoder jv' of
            Ok x -> objDecoder' (Just x) jv
            Err msg -> Err msg
          Nothing -> objDecoder' Nothing jv

succeed :: a -> Decoder a
succeed x _ = Ok x

type Date = (Int, Int, Int)
type Meeting = (Int, String, Date, Date, Maybe String, Maybe String, Maybe String)

decodeDate :: Decoder Date
decodeDate = reqField "day" int (reqField "month" int (reqField "year" int (succeed mkDate)))
  where mkDate y m d = (y, m, d)

decodeMeeting :: Decoder Meeting
decodeMeeting =
  optField "group-photo" string (
    optField "image" string (
      optField "minutes" string (
        reqField "stop" decodeDate (
          reqField "start" decodeDate (
            reqField "location" string (
              reqField "id" int (
                succeed mkMeeting)))))))
  where mkMeeting i loc d1 d2 m im g = (i, loc, d1, d2, m, im, g)

decodeAll :: Value -> Result [Meeting]
decodeAll = decode (array decodeMeeting)

dat :: Value
dat = ARRAY
  [ OBJECT [ ("id", INT 42)
           , ("location", STRING "Utrecht, Netherlands")
           , ("start", OBJECT [("year", INT 2024), ("month", INT 4), ("day", INT 22)])
           , ("stop", OBJECT [("year", INT 2024), ("month", INT 4), ("day", INT 26)])
           , ("minutes", STRING "utrecht")
           , ("image", STRING "utrecht-2024.jpg")
           , ("group-photo", STRING "group-photo.jpg") ]
  , OBJECT [ ("id", INT 41)
           , ("location", STRING "Cambridge, England, UK")
           , ("start", OBJECT [("year", INT 2023), ("month", INT 4), ("day", INT 17)])
           , ("stop", OBJECT [("year", INT 2023), ("month", INT 4), ("day", INT 21)])
           , ("minutes", STRING "cambridge")
           , ("image", STRING "cambridge-2023.jpg") ] ]

check :: Result [Meeting] -> String
check (Ok [(42, _, (2024, 4, 22), (2024, 4, 26), _, _, _), (41, _, (2023, 4, 17), (2023, 4, 21), _, _, _)]) = "pass"
check _ = "fail"

main :: IO ()
main = do
  n <- iterations 1000000
  putStrLn (check (loopLast n (\i -> decodeAll (vary i dat NULL))))
