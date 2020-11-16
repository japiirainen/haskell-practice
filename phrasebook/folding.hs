module Folds where

import Data.Foldable hiding (sum)

mySum :: [Integer] -> Integer
mySum = foldr (+) 0

commaList :: [String] -> String
commaList = foldr commaSep ""
    where
        commaSep x "" = x
        commaSep x phrase = x <> ", " <> phrase

bulletList :: [String] -> String
bulletList = foldMap bulletItem
    where
        bulletItem x = " -" <> x <> "\n"

smashTogether :: [String] -> String
smashTogether = fold

main :: IO ()
main = do
    print $ show nums
    print $ show $ sum nums
    print $ commaList words
    putStr $ bulletList words
    print $ smashTogether words
        where
            nums = enumFromTo 1 10
            words = ["lol", "lmao", ":DD"]
