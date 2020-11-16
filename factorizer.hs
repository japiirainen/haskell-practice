module Factorizer where

factorizer :: (a -> Integer) -> (b -> Integer) -> Either a b -> Integer
factorizer f g (Left x) = f x
factorizer f g (Right y) = g y
