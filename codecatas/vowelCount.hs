module VowelCount where

isVowel :: Char -> Bool
isVowel v =  case v of  
    'a' -> True
    'e' -> True
    'i' -> True
    'o' -> True
    'u' -> True
    _   -> False 


getCount :: String -> Int
getCount  = length . filter isVowel 


better :: String -> Int
better = length . filter (`elem` "aeiou")
