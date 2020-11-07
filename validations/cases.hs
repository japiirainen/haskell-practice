module Cases where

import Data.List
import Data.Char (isAlpha)

isAnagram :: String -> String -> Bool
isAnagram x y = sort x == sort y

isWord :: String -> Maybe String
isWord x = 
    case null x of
        True -> Nothing
        False -> 
            case (all isAlpha x) of
                True -> Just x
                False -> Nothing

areAnagrams :: String -> String -> String
areAnagrams x y = 
    case isWord x of
        Nothing -> "The first word is invalid!"
        Just x  -> 
            case isWord y of
                Nothing -> "The second word is invalid!"
                Just y  -> 
                    case isAnagram x y of
                        False -> "These words are not anagrams"
                        True  -> "These words are anagrams"


main :: IO ()
main = do
    putStrLn "Please enter a word"
    word1 <- getLine
    putStrLn "Please enter a second word"
    word2 <- getLine
    print $ areAnagrams word1 word2
    
