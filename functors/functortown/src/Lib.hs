module Lib where

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair l r) = Pair (f l) (f r)

data IncrementPair a =
    IncrementPair Integer a deriving (Eq, Show)

instance Functor IncrementPair where
    fmap f (IncrementPair int r) = IncrementPair (int + 1) (f r)


someFunc :: IO ()
someFunc = putStrLn "someFunc"
