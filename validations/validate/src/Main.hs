module Main where

import Data.Char (isAlphaNum, isSpace)
import Data.Semigroup()
import Data.Coerce
import Data.Validation hiding (Validate, _Validation)
import Control.Lens

class Validate f where
    _Validation :: Iso (f e a) (f g b) (Validation e a)
                                       (Validation g b)

instance Validate Either where
    _Validation =
        iso (\either -> case either of
                Left x -> Failure x
                Right x -> Success x)
            (\validation -> case validation of
                Failure x -> Left x
                Success x -> Right x)

instance Validate Validation where
    _Validation = iso id id


newtype Password = Password String deriving (Eq, Show)

newtype Error =
    Error [String] deriving (Eq, Show)


newtype Username = Username String deriving (Eq, Show)

data User = User Username Password deriving (Eq, Show)

type Rule a = a -> Validation Error a

instance Semigroup Error where
    Error xs <> Error ys = Error (ys <> xs)

class LiftAB f where
    liftA :: a -> f a b
    liftB :: b -> f a b

instance LiftAB Validation where
    liftA = Failure
    liftB = Success

instance LiftAB Either where
    liftA = Left
    liftB = Right

class FoldAB f where
    foldAB :: (a -> c) -> (b -> c) -> f a b -> c

instance FoldAB Either where
    foldAB = either

instance FoldAB Validation where
    foldAB = validation

passwordLength :: String -> Validation Error Password
passwordLength "" = Failure $ Error ["Password cannot be empty"]
passwordLength password = 
    if length password < 20 then
        Success $ Password password 
        else Failure $ Error ["Password is too long ..."]

usernameLength :: String -> Validation Error Username
usernameLength name = 
    if length name > 15 then
        Failure $ Error ["Username is too long..."]
        else Success $ Username name

isAlpha :: Rule String
isAlpha "" = Failure $ Error ["Password cannot be empty"]
isAlpha xs = 
    if all isAlphaNum xs then
        Success xs
        else Failure $ Error ["Password cannot contain special characters"]

stripSpace :: Rule String
stripSpace "" = Failure $ Error ["Password cannot be empty"]
stripSpace (x:xs) =
    if isSpace x then
        stripSpace xs
        else Success (x:xs)

validatePassword :: Rule Password
validatePassword (Password xs) =
    case stripSpace xs of
        Failure err -> Failure err
        Success xs  -> isAlpha xs *> passwordLength xs

validateUsername :: Rule Username
validateUsername (Username xs) = 
    case stripSpace xs of
        Failure err -> Failure err
        Success xs  -> isAlpha xs *> usernameLength xs

mapFailure :: (e1 -> e2) -> Validation e1 a -> Validation e2 a
mapFailure f (Failure e) = Failure $ f e
mapFailure _ (Success x) = Success x


passwordErrors :: Rule Password
passwordErrors xs = 
        over _Failure (\err -> Error ["Invalid password: "] <> err)
                   (validatePassword xs)

usernameErrors :: Rule Username
usernameErrors xs = 
    case validateUsername xs of
        Failure err -> Failure $ Error ["Invalid username: "] <> err
        Success username -> Success username

makeUser :: Validate v => Username -> Password -> v Error User
makeUser username password = 
         review _Validation $
            User <$> usernameErrors username <*> passwordErrors password

display :: Username -> Password -> IO ()
display name password =
    case makeUser name password of
        Left err -> putStrLn $ unlines $ coerce err
        Right (User (Username name) _) -> putStrLn $ "Welcome, " <> name 

main :: IO ()
main = do
  putStrLn "Please enter a username"
  username <- Username <$> getLine
  putStrLn "Please enter a password"
  password <- Password <$> getLine
  display username password
