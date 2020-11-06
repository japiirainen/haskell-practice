module Palindromes where 

isPalindrome :: String -> Bool
isPalindrome a = a == reverse a
