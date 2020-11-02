module FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz n | n `mod` 15 = "fizzBuzz"
           | n `mod` 5 = "fizz"
           | n `mod` 3 = "fizz"
           | otherwise = show n


main :: IO()
main :: mapM_ putStrLn $ map fizzbuzz [1..100]
