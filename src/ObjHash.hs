module ObjHash(hashString)
where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Lazy as Byte
import qualified Data.Aeson.Types as T
import qualified Data.Scientific as Sci
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import Data.Digest.Pure.SHA (Digest, SHA256State, sha256, showDigest)

hash :: String -> Byte.ByteString -> String
hash s i = showDigest $ sha256 $ Byte.concat [Char8.pack s, i]

hashString :: String -> String
hashString s = hash "u" (Char8.pack s)

hashInteger :: Int -> String
hashInteger i = ""

hashFloat :: Double -> String
hashFloat d = ""

hashNumber :: Sci.Scientific -> String
hashNumber n =
  if Sci.isInteger n then  n else hashFloat n

hashNull :: String
hashNull = hashString ""

hashBool::Bool -> String
hashBool b =
  let bString = if b then "1" else "0" in
  hash "b" (Char8.pack bString)


hashList::T.Array -> String
hashList a = concat $ Vector.map hashValue a

hashValue :: T.Value -> String
hashValue T.Null = hashNull
hashValue (T.String s) = hashString $ Text.unpack s
hashValue (T.Bool b) = hashBool b
hashValue (T.Array xs) = hashList xs
hashValue (T.Number n) = hashNumber n
