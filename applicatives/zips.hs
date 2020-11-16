{-# LANGUAGE DeriveFunctor #-}
module Zips where

import Control.Applicative hiding (ZipList (..))

zipFunc :: [a] -> [b] -> [(a, b)]
zipFunc _ [] = []
zipFunc [] _ = []
zipFunc (x:xs) (y:ys) = (x,y) : zipFunc xs ys

zipApp :: [a -> b] -> [a] -> [b]
zipApp _ [] = []
zipApp [] _ = []
zipApp (f:fs) (x:xs) = f x : zipApp fs xs

zipApply :: [a -> b] -> [a] -> [b]
zipApply xs ys = (\(f, x) -> f x) <$> zip xs ys

-- ex1: write zipWith from Control.Applicative
zippWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zippWith _ [] _ = []
zippWith _ _ [] = []
zippWith f (x:xs) (y:ys) = f x y : zippWith f xs ys

-- ex2: write Applicative instance for ZipList

newtype ZipList a = ZipList { getZiplist :: [a] }
    deriving (Eq, Show, Functor)

instance Applicative ZipList where
    pure x = ZipList $ repeat x
    liftA2 f (ZipList xs) (ZipList ys) = ZipList $ zipWith f xs ys
    ZipList _ <*> ZipList [] = ZipList []
    ZipList (f:fs) <*> ZipList (x:xs) =
        ZipList (f x : getZiplist (ZipList fs <*> ZipList xs))





