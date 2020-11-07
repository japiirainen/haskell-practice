module Folds where

import Data.Char

rejectNonAlphabethical :: String -> Maybe String
rejectNonAlphabethical x = 
    case (foldAll isAlpha x) of
        False -> Nothing
        True  -> Just x


myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll pred (x:xs) = 
    case pred x of
        False -> False
        True -> myAll pred xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x:xs) =
    case pred x of
        True -> True
        False -> myAny pred xs

-- own implementations of folds

myFoldR :: (a -> b -> b) -> b -> [a] -> b
myFoldR f acc [] = acc
myFoldR f acc (x:xs) = f x (foldr f acc xs)

myFoldL :: (b -> a -> b) -> b -> [a] -> b
myFoldL f acc [] = acc
myFoldL f acc (x:xs) = myFoldL f (f acc x) xs

-- first section with folds

foldAll :: (a -> Bool) -> [a] -> Bool
foldAll pred = foldr (\x y -> pred x && y) True

foldAny :: (a -> Bool) -> [a] -> Bool
foldAny pred = foldr (\x y -> pred x|| y) False

myProduct :: Num a => [a] -> a
myProduct list = foldl (*) 1 list
