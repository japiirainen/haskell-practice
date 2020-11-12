module MoreThanASum where

import Data.Bifunctor

data These a b = This a | That b | These a b
    deriving Show

instance Functor (These a) where
    fmap f (These l r) = These l (f r)

instance Bifunctor These where
    bimap f g (These l r) = These (f l) (g r)

main :: IO ()
main = print "lol"
