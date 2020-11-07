module Lesson1 where

function :: (Ord a, Num a) => a -> a -> a
function x y = if x > y then x * 10 else y

function2 :: (Ord a, Num a) => a -> a -> a
function2 x y =
    case x > y of
        False -> y
        True  -> x * 10
