{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import           Control.Algebra
import           Control.Applicative
import           Control.Carrier.Error.Either
import           Control.Carrier.Interpret
import           Control.Carrier.Reader
import           Control.Exception            (throwIO)
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as L
import           Data.Foldable                (traverse_)
import           Data.Kind                    (Type)
import           Data.Time.Clock
import qualified Network.HTTP.Client          as HTTP
import           Network.HTTP.Client.Internal (Response (..),
                                               ResponseClose (..))
import qualified Network.HTTP.Client.TLS      as HTTP
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Version


newtype CatFact = CatFact
    { catFact :: String
    } deriving (Show)

instance FromJSON CatFact where
    parseJSON = withObject "CatFact" $ \o -> CatFact <$> o .: "text"

data CatFactClient (m :: Type -> Type) k where
    ListFacts :: Int -> CatFactClient m [CatFact]

listFacts :: Has CatFactClient sig m => Int -> m [CatFact]
listFacts n = send (ListFacts n)

data Http (m :: Type -> Type) k where
    SendRequest :: HTTP.Request -> Http m (HTTP.Response L.ByteString)

sendRequest :: Has Http sig m => HTTP.Request -> m (HTTP.Response L.ByteString)
sendRequest r = send (SendRequest r)

newtype JsonParseError = JsonParseError String
    deriving (Show, Eq)

newtype InvalidContentType = InvalidContentType String
    deriving (Show, Eq)

decodeOrThrow :: (Has (Throw JsonParseError) sig m, FromJSON a) => L.ByteString -> m a
decodeOrThrow = either (throwError . JsonParseError) pure . eitherDecode

newtype CatFactsApi m a = CatFactsApi { runCatFactsApi :: m a }
    deriving ( Monad
             , Functor
             , Applicative
             , MonadIO
             , Alternative
             )

catFactsEndpoint :: HTTP.Request
catFactsEndpoint = HTTP.parseRequest_ "https://cat-fact.herokuapp.com/facts/random"

instance ( Has Http sig m
         , Has (Throw JsonParseError) sig m
         , Has (Throw InvalidContentType) sig m
         , Algebra sig m
         ) => Algebra (CatFactClient :+: sig) (CatFactsApi m) where
    alg hdl sig ctx = case sig of
        L (ListFacts numberOfFacts) -> do
            resp <- sendRequest (catFactsEndpoint { HTTP.queryString = "?amount=" <> B.pack (show numberOfFacts) })
            case lookup hContentType (HTTP.responseHeaders resp) of
                Just "application/json; charset=utf-8" -> (<$ ctx) <$> decodeOrThrow (HTTP.responseBody resp)
                other -> throwError (InvalidContentType (show other))
        R other -> CatFactsApi (alg (runCatFactsApi . hdl) other ctx)

newtype HttpClient m a = HttpClient { runHttp :: m a }
    deriving ( Monad
             , Functor
             , Applicative
             , MonadIO
             , Alternative
             )

instance (MonadIO m, Algebra sig m) => Algebra (Http :+: sig) (HttpClient m) where
    alg hdl sig ctx = case sig of
        L (SendRequest req) -> (<$ ctx) <$> liftIO (HTTP.getGlobalManager >>= HTTP.httpLbs req)
        R other -> HttpClient (alg (runHttp . hdl) other ctx)

handlePrint :: Either InvalidContentType (Either JsonParseError [CatFact]) -> IO ()
handlePrint = \case
    Left invalidContentTypeError -> print invalidContentTypeError
    Right ok -> case ok of
        Left jsonParseError -> print jsonParseError
        Right facts         -> traverse_ print facts

catFactsRunner :: Has Http sig m => m (Either InvalidContentType (Either JsonParseError [CatFact]))
catFactsRunner =
    runError @InvalidContentType $
    runError @ JsonParseError $
    runCatFactsApi $
    listFacts 5

main :: IO ()
main = do
    resp <- runHttp catFactsRunner
    handlePrint resp
