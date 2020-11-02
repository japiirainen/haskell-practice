module FizzBuzz where

import prelude ()

fizzbuzz :: Int -> String
fizzbuzz n | n `mod` 15 = "fizzBuzz"
           | n `mod` 5 = "fizz"
           | n `mod` 3 = "fizz"
           | otherwise = show n
