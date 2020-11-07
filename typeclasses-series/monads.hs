module Monads where

import Data.Char

database :: [(Integer, String)]
database = [(1, "Jontte"),
            (2, "Manakine"),
            (3, "Kanakkune"),
            (4, "J00li3")]

greetUser :: Integer -> Maybe String
greetUser record = fmap ("Hello, " ++) (lookup record database >>= makeUsername)

rejectNonAlphabetic :: String -> Maybe String
rejectNonAlphabetic x =
    case all isAlpha x of
        False -> Nothing
        True  -> Just x

removeSpaces :: String -> Maybe String
removeSpaces x =
    case (filter (\x -> not (x == ' ')) x) of
        "" -> Nothing
        res -> Just res

validateLength :: String -> Maybe String
validateLength x =
    case (length x > 25) of
        True -> Nothing
        False -> Just x

makeUsername :: String -> Maybe String
makeUsername x = removeSpaces x >>= rejectNonAlphabetic >>= validateLength
