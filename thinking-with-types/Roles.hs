{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE RoleAnnotations #-}
module Roles where

import           Data.Coerce                    ( Coercible(..)
                                                , coerce
                                                )
import           Data.Foldable                  ( toList )
import qualified Data.Map                      as M
import           Data.Monoid                    ( Product(..)
                                                , Sum(..)
                                                )

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce


newtype Reverse a = Reverse
 { getReverse :: a
 } deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
    compare (Reverse a) (Reverse b) = compare b a


type family IntToBool a where
    IntToBool Int = Bool
    IntToBool a = a


data BST v
    = Empty
    | Branch (BST v) v (BST v)

type role BST nominal
