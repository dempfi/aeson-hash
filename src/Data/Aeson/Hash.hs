module Data.Aeson.Hash
  ( hash
  )
where

import           Data.List                      ( sort )
import           Data.Aeson                     ( ToJSON
                                                , toJSON
                                                )
import qualified Data.Text                     as Text
import qualified Data.Vector                   as Vector
import qualified Data.ByteString               as BS
import qualified Data.Aeson.Types              as Types
import qualified Crypto.Hash.SHA256            as SHA256
import           Data.Scientific                ( Scientific
                                                , toRealFloat
                                                )
import           Data.HashMap.Strict            ( toList )
import           Data.ByteString.Char8          ( pack )

sha256 :: String -> BS.ByteString -> BS.ByteString
sha256 prefix bs = SHA256.hash $ BS.append (pack prefix) bs

hashString :: Text.Text -> BS.ByteString
hashString s = sha256 "u" $ pack $ Text.unpack s

hashNull :: BS.ByteString
hashNull = BS.empty

hashBool :: Bool -> BS.ByteString
hashBool b = sha256 "b" $ pack bs where bs = if b then "1" else "0"

significandString :: Double -> String
significandString d | d == 0    = ""
                    | d >= 1    = "1" ++ significandString ((d - 1) * 2)
                    | otherwise = "0" ++ significandString (d * 2)

-- Specific to object hash, wrong (exp 1 = 0) exponent
customExponent :: Double -> Int -> Int
customExponent m n | m > 1     = customExponent (m / 2) (n + 1)
                   | m < 0.5   = customExponent (m * 2) (n - 1)
                   | otherwise = n

-- Specific to object hash, not mathematically accurate
customSignificand :: Double -> Double
customSignificand m | m > 1     = customSignificand (m / 2)
                    | m < 0.5   = customSignificand (m * 2)
                    | otherwise = m

normalizeDouble :: Double -> String
normalizeDouble d | isInfinite d = if d > 0 then "Infinity" else "-Infinity"
                  | isNaN d      = "NaN"
                  | d == 0       = "+0:"
                  | d < 0        = "-" ++ stringified (-d)
                  | d > 0        = "+" ++ stringified d
                  | otherwise    = "NaN"
 where
  stringified n =
    show (customExponent n 0) ++ ":" ++ significandString (customSignificand n)

hashNumber :: Scientific -> BS.ByteString
hashNumber n = sha256 "f" $ pack $ normalizeDouble $ toRealFloat n

hashArray :: Types.Array -> BS.ByteString
hashArray arr = sha256 "l" $ BS.concat bsList
  where bsList = Vector.toList $ Vector.map hashValue arr

hashObject :: Types.Object -> BS.ByteString
hashObject obj = sha256 "d" $ BS.concat $ sort hashedElements
 where
  hashElement (k, v) = BS.append (hashString k) (hashValue v)

  hashedElements = map hashElement $ toList obj

hashValue :: Types.Value -> BS.ByteString
hashValue Types.Null       = hashNull
hashValue (Types.Bool   b) = hashBool b
hashValue (Types.Array  a) = hashArray a
hashValue (Types.String s) = hashString s
hashValue (Types.Number n) = hashNumber n
hashValue (Types.Object o) = hashObject o

hash :: ToJSON a => a -> BS.ByteString
hash = hashValue . toJSON
