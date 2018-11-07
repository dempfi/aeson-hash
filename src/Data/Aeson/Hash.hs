module Data.Aeson.Hash
  (
    hash
  , hashNull
  , hashBool
  , hashNumber
  , hashArray
  , hashString
  ) where

import Data.List (sort)
import Data.Aeson (ToJSON, toJSON)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.Aeson.Types as Types
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Scientific (Scientific, floatingOrInteger)
import Data.HashMap.Strict (toList)
import Data.ByteString.Char8 (pack)

sha256 :: String -> BS.ByteString -> BS.ByteString
sha256 prefix bs = SHA256.hash $ BS.append (pack prefix) bs

hashString :: Text.Text -> BS.ByteString
hashString s = sha256 "u" $ pack $ Text.unpack s

hashNull :: BS.ByteString
hashNull = BS.empty

hashBool :: Bool -> BS.ByteString
hashBool b = sha256 "b" $ pack bs
  where
    bs = if b then "1" else "0"

significandString :: Double -> String
significandString d
  | d == 0    = ""
  | d >= 1    = "1" ++ significandString ((d - 1) * 2)
  | otherwise = "0" ++ significandString (d * 2)

normalizeDouble :: Double -> String
normalizeDouble d
  | isInfinite d = if d > 0 then "Infinity" else "-Infinity"
  | isNaN d      = "NaN"
  | d == 0       = "+0:"
  | d < 0        = "-" ++ stringified (-d)
  | d > 0        = "+" ++ stringified d
  where
    stringified n =
      show (exponent n) ++ ":" ++ significandString (significand n)

hashNumber :: Scientific -> BS.ByteString
hashNumber sci =
  case num of
    Left double -> sha256 "f" $ pack $ normalizeDouble double
    Right int -> sha256 "i" $ pack $ show int
  where
    num = floatingOrInteger sci

hashArray :: Types.Array -> BS.ByteString
hashArray _ = BS.empty
-- hashArray a = concat $ Vector.map hashValue a

hashObject :: Types.Object -> BS.ByteString
hashObject obj = sha256 "d" $ BS.concat $ sort hashedElements
  where
    hashElement (k, v) = BS.append (hashString k) (hashValue v)
    hashedElements = map hashElement $ toList obj

hashValue :: Types.Value -> BS.ByteString
hashValue Types.Null = hashNull
hashValue (Types.Bool b) = hashBool b
hashValue (Types.String s) = hashString s
hashValue (Types.Array xs) = hashArray xs
hashValue (Types.Number n) = hashNumber n
hashValue (Types.Object o) = hashObject o

hash :: ToJSON a => a -> BS.ByteString
hash = hashValue . toJSON
