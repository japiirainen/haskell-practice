{-# LANGUAGE FlexibleInstances #-}
module Main where
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State    (MonadState (..), State, modify, void)

class Monad m => MonadLogger m where
    logInfo :: String -> m ()

instance MonadLogger IO where
  logInfo = putStrLn

instance MonadLogger (State String) where
    logInfo = put

main :: IO ()
main = logInfo "foobar"
