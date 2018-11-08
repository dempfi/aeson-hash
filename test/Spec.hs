{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson.Hash
import Data.Aeson
import GHC.Generics
import Numeric (showHex)
import qualified Data.ByteString as BS

data Person = Person {
    name :: String
  , age  :: [Double]
  } deriving (Generic, Show)

instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions

prettyPrint :: BS.ByteString -> String
prettyPrint = concat . map (flip showHex "") . BS.unpack

main :: IO ()
main = print $ prettyPrint $ hash (Person {name = "Joe", age = [12.10]})
