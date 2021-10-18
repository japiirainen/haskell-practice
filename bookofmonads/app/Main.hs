module Main where
import           Control.Applicative
import           Data.Foldable       (traverse_)

type FirstName = String
type LastName = String

data Person = Person FirstName LastName
    deriving (Show)

validateFirstName :: String -> Maybe FirstName
validateFirstName []        = Nothing
validateFirstName firstName = Just firstName

validateLastName :: String -> Maybe LastName
validateLastName []       = Nothing
validateLastName lastName = Just lastName

validatePerson :: String -> String -> Maybe Person
validatePerson firstName lastName = liftA2 Person (validateFirstName firstName) (validateLastName lastName)

validatePerson2 :: String -> String -> Maybe Person
validatePerson2 firstName lastName = Person <$> validateFirstName firstName <*> validateLastName lastName

main :: IO ()
main = traverse_ print [validatePerson "John" "Baz", validatePerson2 "" "Baz"]
