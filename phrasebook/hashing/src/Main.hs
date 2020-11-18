{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where

import Data.Hashable (Hashable (hash))
import qualified Crypto.Hash as Crypto
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteArray.Encoding
import Data.Word (Word8)
import GHC.Generics (Generic)

data Color =
    Color { red   :: Word8
          , green :: Word8
          , blue  :: Word8
          } deriving stock (Show, Generic) 
            deriving anyclass (Hashable)

sha256 :: String -> String
sha256 input = result 
    where
        bytes  = UTF8.fromString input
        digest = Crypto.hashWith Crypto.SHA256 bytes
        hex    = convertToBase Base16 digest
        result = UTF8.toString hex

main :: IO ()
main = do
    putStrLn $ "some color hashed... :  " <> show color <> "\n\n"
    putStrLn $ "sha256(abc)     =  " <> sha256 "abc" <> "\n\n"
    putStrLn $ "sha256(hello)   =  " <> sha256 "hello"
        where 
            color = hash $ Color 0 255 0
