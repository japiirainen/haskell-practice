module Monoids where

import Data.Foldable

newtype Sum a = Sum a
newtype Product a = Product a

-- Sum 5 <> Sum 10 = Sum 15
-- Product 10 <> Product 5 = Product 50

-- Monoid always has
-- (<>) :: a -> a -> a
-- mempty :: a

example1 :: Maybe String
example1 = Nothing

example2 :: Maybe String
example2 = Just "Joona"
