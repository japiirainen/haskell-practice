module BifunctorIntro where

import Data.Bifunctor

data IncrementPair a b =
  IncrementPair a b deriving (Show, Eq)


instance Bifunctor IncrementPair where
    bimap f g (IncrementPair int r) = 
        IncrementPair (f int) (g r)

someBimap :: (Num a, Num b) => (a,b) -> (a,b)
someBimap (l,r) = bimap (+10) (*10) (l,r)

someFirst :: (Num a) => (a,b) -> (a,b)
someFirst = first (*100) 

someSecond :: (Num b) => (a,b) -> (a,b)
someSecond = second (*100) 
