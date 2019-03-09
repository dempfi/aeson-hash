{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

import           Data.Aeson
import           GHC.Generics
import           Test.Hspec
import           Text.Printf                    ( printf )
import           Data.Aeson.Hash                ( hash )
import qualified Data.ByteString               as BS

h :: ToJSON a => a -> String
h = concatMap (printf "%02x") . BS.unpack . hash

newtype Doubles = Doubles [Double] deriving (Generic, Show)
instance ToJSON Doubles where
  toEncoding = genericToEncoding defaultOptions

newtype Strings = Strings [String] deriving (Generic, Show)
instance ToJSON Strings where
  toEncoding = genericToEncoding defaultOptions

newtype Ints = Ints [Int] deriving (Generic, Show)
instance ToJSON Ints where
  toEncoding = genericToEncoding defaultOptions

newtype Booleans = Booleans [Bool] deriving (Generic, Show)
instance ToJSON Booleans where
  toEncoding = genericToEncoding defaultOptions

newtype Obj1 = Obj1 { foo :: String } deriving (Generic, Show)
instance ToJSON Obj1 where
  toEncoding = genericToEncoding defaultOptions

data Obj2 = Obj2 { foo :: [String], qux :: [String] } deriving (Generic, Show)
instance ToJSON Obj2 where
  toEncoding = genericToEncoding defaultOptions

main :: IO ()
main = hspec $ describe "Data.Aeson.Hash" $ do
  context "List" $ do
    context "Of strings" $ do
      specify "Empty"
        $ h (Strings [])
        `shouldBe` "acac86c0e609ca906f632b0e2dacccb2b77d22b0621f20ebece1a4835b93f6f0"

      specify "Singleton"
        $ h (Strings ["foo"])
        `shouldBe` "268bc27d4974d9d576222e4cdbb8f7c6bd6791894098645a19eeca9c102d0964"

      specify "Simple"
        $ h (Strings ["foo", "bar"])
        `shouldBe` "32ae896c413cfdc79eec68be9139c86ded8b279238467c216cf2bec4d5f1e4a2"

    context "Of ints" $ do
      specify "Singleton"
        $ h (Ints [123])
        `shouldBe` "2e72db006266ed9cdaa353aa22b9213e8a3c69c838349437c06896b1b34cee36"

      specify "Simple"
        $ h (Ints [0, 1, 2, 3])
        `shouldBe` "9c6a042b7dfd0819735a839409456b303fff77c3416c292c3ba61b9a96336fac"

      specify "Singleton with big int"
        $ h (Ints [123456789012345])
        `shouldBe` "f446de5475e2f24c0a2b0cd87350927f0a2870d1bb9cbaa794e789806e4c0836"

      specify "Simple with big ints"
        $ h (Ints [123456789012345, 678901234567890])
        `shouldBe` "d4cca471f1c68f62fbc815b88effa7e52e79d110419a7c64c1ebb107b07f7f56"

    context "Of booleans" $ do
      specify "Singleton true"
        $ h (Booleans [True])
        `shouldBe` "c9757fccb1537e176de1d7fe6b3677075640873990ae195de4e2df715744ce61"

      specify "Singleton false"
        $ h (Booleans [False])
        `shouldBe` "3f290378c94f4cc25a20b2345b418ef48628a0cdde1a7aa8a6138a3cea180f9e"

      specify "Simple"
        $ h (Booleans [True, False, False])
        `shouldBe` "2e27cc33925f9aae88d48ebb074dec31da423a1d379a223833af160c7f841518"

  context "Objects" $ do
    specify "Simple"
      $ h (Obj1 { foo = "bar" })
      `shouldBe` "7ef5237c3027d6c58100afadf37796b3d351025cf28038280147d42fdc53b960"

    specify "Complex"
      $ h (Obj2 { foo = ["bar", "baz"], qux = ["norf"] })
      `shouldBe` "f1a9389f27558538a064f3cc250f8686a0cebb85f1cab7f4d4dcc416ceda3c92"
