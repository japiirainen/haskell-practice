{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson            (Value, encode)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Lib
import           Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpJSON "https://jsonplaceholder.typicode.com/posts/1"

    putStrLn $ "status => " ++ show (getResponseStatusCode response)

    print (getResponseBody response :: Value)
