module Main where

import Data.Char (isAlphaNum, isSpace)

maxLength :: String -> Maybe String
maxLength "" = Nothing
maxLength xs = 
    case length xs < 20 of
        True -> Just xs
        False -> Nothing

isAlpha :: String -> Maybe String
isAlpha "" = Nothing
isAlpha xs = 
    case all isAlphaNum xs of
        True -> Just xs
        False -> Nothing

stripSpace :: String -> Maybe String
stripSpace "" = Nothing
stripSpace (x:xs) =
    case isSpace x of
        True -> stripSpace xs
        False -> Just (x:xs)

validate :: String -> Maybe String
validate "" = Nothing
validate xs = 
    case stripSpace xs of
        Nothing -> Nothing
        Just xs ->
            case isAlpha xs of
                Nothing -> Nothing
                Just xs -> 
                    case maxLength xs of
                        Nothing -> Nothing
                        Just xs -> Just xs

main :: IO ()
main = do
  putStrLn "Please enter a password"
  password <- getLine
  print $ validate password
