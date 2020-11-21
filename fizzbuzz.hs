module FizzBuzz where
import Data.Foldable

fizzle :: Int -> String
fizzle x 
    | x `mod` 15 == 0 = "FizzBuzz"
    | x `mod` 5  == 0 = "Buzz"
    | x `mod` 3  == 0 = "Fizz"
    | otherwise = show x

main :: IO ()
main = do
    for_ [1..100] $ print . fizzle
