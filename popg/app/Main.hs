{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import           Data.Maybe
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.Resource

data Teletype m a where
    ReadTTY :: Teletype m String
    WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret  \case
    ReadTTY      -> embed getLine
    WriteTTY msg -> embed $ putStrLn msg

echo :: Member Teletype r => Sem r ()
echo = do
    i <- readTTY
    case i of
        "" -> pure ()
        _  -> writeTTY i >> echo


runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i =
     runOutputMonoid pure
   . runInputList i
   . reinterpret2 \case
        ReadTTY      -> fromMaybe "" <$> input
        WriteTTY msg -> output msg

echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeletypePure echo

pureOutput :: [String] -> [String]
pureOutput = fst . run . echoPure

data CustomException = ThisException | ThatException
    deriving (Show, Eq)

program :: Members '[Resource, Teletype, Error CustomException] r => Sem r ()
program = catch @CustomException work \e -> writeTTY $ "Caught" <> show e
    where
        work = bracket readTTY (const $ writeTTY "exiting bracket") \input -> do
            case input of
                "explode"     -> throw ThisException
                "weird stuff" -> writeTTY input *> throw ThatException
                _             -> writeTTY input *> writeTTY "no exceptions"

main :: IO (Either CustomException ())
main
    = runFinal
    . embedToFinal @IO
    . resourceToIOFinal
    . errorToIOFinal @CustomException
    . teletypeToIO
    $ program
