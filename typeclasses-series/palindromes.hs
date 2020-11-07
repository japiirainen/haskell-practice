module Palindromes where 

import Pal (isPalindrome)

-- Interactive program
main :: IO ()
main = do
    putStrLn "type something to check if it's a palindrome"
    word <- getLine
    print (verbose word)

verbose :: String -> String
verbose a = 
    case isPalindrome a of
    Just True -> "That is a palindrome"
    Just False -> "That is not a palindrome"
    Nothing -> "please enter some input"
