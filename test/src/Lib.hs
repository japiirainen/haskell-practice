module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello world!"

add :: Num a => a -> a -> a
add x y = x + y

double x = x + x

quadruple x = double ( double x )

factorial n = product [1 .. n]

average ns = sum ns `div` length ns

poly x = let
    y = y + 1
    in y * y

getEvens xs = filter (\x -> x `mod` 2 == 0) xs


