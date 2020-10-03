module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello world!"

add :: Num a => a -> a -> a
add x y = x + y

double x = x + x

quadruple :: Num a => a -> a
quadruple x = double ( double x )

factorial n = product [1 .. n]

average ns = sum ns `div` length ns

poly x = let
    y = y + 1
    in y * y

getEvens xs = filter (\x -> x `mod` 2 == 0) xs


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [a | a <- xs, a <= x]
                    larger  = [b | b <- xs, b > x]
                    