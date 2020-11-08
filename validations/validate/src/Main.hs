module Main where

import Data.Char (isAlphaNum, isSpace)
import Data.Validation
newtype Password = Password String deriving (Eq, Show)
newtype Error = Error String deriving (Eq, Show)
newtype Username = Username String deriving (Eq, Show)
data User = User Username Password deriving (Eq, Show)

passwordLength :: String -> Either Error Password
passwordLength "" = Left $ Error "Password cannot be empty"
passwordLength password = 
    case length password < 20 of
        True -> Right $ Password password
        False -> Left $ Error "Password is too long ..."

usernameLength :: String -> Either Error Username
usernameLength name = 
    case length name > 15 of
        True -> Left $ Error "Username is too long..."
        False -> Right $ Username name

isAlpha :: String -> Either Error String
isAlpha "" = Left $ Error "Password cannot be empty"
isAlpha xs = 
    case all isAlphaNum xs of
        True -> Right xs
        False -> Left $ Error "Password cannot contain special characters"

stripSpace :: String -> Either Error String
stripSpace "" = Left $ Error "Password cannot be empty"
stripSpace (x:xs) =
    case isSpace x of
        True -> stripSpace xs
        False -> Right (x:xs)

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
validatePassword :: Password -> Either Error Password
validatePassword (Password xs) =
    stripSpace xs
    >>= isAlpha
    >>= passwordLength

validateUsername :: Username -> Either Error Username
validateUsername (Username xs) = 
    stripSpace xs
    >>= isAlpha
    >>= usernameLength

makeUser :: Username -> Password -> Either Error User
makeUser username password = 
    User <$> validateUsername username 
         <*> validatePassword password

main :: IO ()
main = do
  putStrLn "Please enter a username"
  username <- Username <$> getLine
  putStrLn "Please enter a password"
  password <- Password <$> getLine
  print $ makeUser username password
