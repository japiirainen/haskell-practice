module Main where

import Data.List (sortBy)
import Data.Function (on)
import Numeric.Natural


data User = User { name :: String
                 , surname :: String
                 , age :: Natural
                 , beta :: Bool
                 , admin :: Bool }
            deriving (Show, Eq)

userList :: [User]
userList = [
    User "Julie" "Moronuki" 74 False True,
    User "Chris" "Martin" 25 False True,
    User "Alonzo" "Church" 100 True False,
    User "Alan" "Turing" 99 True False,
    User "Melman" "Fancypants" 0 False False
  ]

fetchUsers :: String -> Maybe [User]
fetchUsers query =
    case query of
      "allUsers"  -> Just userList
      "betaUsers" -> Just $ filter beta userList
      "admins"    -> Just $ filter admin userList
      _           -> Nothing

readSortMaybe :: String -> Maybe ([User] -> [User])
readSortMaybe ordering =
    case ordering of
      "surname" -> Just $ sortBy (compare `on` surname)
      "age"     -> Just $ sortBy (compare `on` age)
      _         -> Nothing

liftAp :: Maybe (a -> b) -> Maybe a -> Maybe b
liftAp _ Nothing = Nothing
liftAp Nothing _ = Nothing
liftAp (Just f) (Just x) = Just (f x)

sortUsers :: String -> String -> Maybe [User]
sortUsers ordering query =
    readSortMaybe ordering
    <*> fetchUsers query

main :: IO ()
main = do
  putStrLn "Well hello everyone"
