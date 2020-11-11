module Lib where

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair l r) = Pair (f l) (f r)

data IncrementPair a =
    IncrementPair Integer a deriving (Eq, Show)

instance Functor IncrementPair where
    fmap f (IncrementPair int r) = IncrementPair (int + 1) (f r)

data BackwardPair a = BackwardPair a a deriving (Eq, Show)

instance Functor BackwardPair where
    fmap f (BackwardPair l r) = BackwardPair (f r) (f l)

composedNum :: (Functor f, Num a) => f a -> f a
composedNum = fmap (abs . (subtract 100))

fcomposedNum :: (Functor f, Num a) => f a -> f a
fcomposedNum = fmap abs . fmap (subtract 100)


