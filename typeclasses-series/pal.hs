module Pal where

import Data.Char

isPalindrome :: String -> Maybe Bool
isPalindrome = isOwnReverseMaybe . rejectEmpty . normalize

normalize :: String -> String
normalize = filter notPunctiation . filter notSpace . map toLower

rejectEmpty :: String -> Maybe String
rejectEmpty a = 
    case a of
    [] -> Nothing
    _  -> Just a

isOwnReverseMaybe :: Maybe String -> Maybe Bool
isOwnReverseMaybe a = 
    case a of
    Just a -> Just (isOwnReverser a)
    Nothing -> Nothing

isOwnReverser :: String -> Bool
isOwnReverser a = a == reverse a

isPalindromeIgnoreCase :: String -> Bool
isPalindromeIgnoreCase = isOwnReverser . allLowerCase

isPalindromePhrase :: String -> Bool
isPalindromePhrase = isOwnReverser . allLowerCase . withoutSpaces

nonEmptyPal :: String -> Maybe Bool
nonEmptyPal a = 
    case a of
    "" -> Nothing
    _  -> Just (isOwnReverser a)


notSpace :: Char -> Bool
notSpace a = a /= ' '

notPunctiation :: Char -> Bool
notPunctiation a = not (isPunctuation a)

allLowerCase :: String -> String
allLowerCase = map toLower

withoutSpaces :: String -> String
withoutSpaces = filter notSpace
