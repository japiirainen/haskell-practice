module MonoidsInFunctors where

import Control.Applicative
import Data.Monoid

data Person = Person {
                  name :: String
                , pl   :: String
                     } deriving (Eq, Show)

--instance Monoid a => Applicative ((,) a) where
--    pure x = (mempty, x)
--    (x, func) <*> (y, z') = (x <> y, func z)
--    liftA2 func (x, z) (y, z') = (x <> y, func z z')

nonEmpty :: String -> Either String String
nonEmpty str =
    case null str of
      True -> Left "Error: Empty string"
      False -> Right str

mkPerson :: String -> String -> Either String Person
mkPerson name' pl' = liftA2 Person (nonEmpty name') (nonEmpty pl')

otherMkPerson :: String -> String -> Either String Person
otherMkPerson name' pl' = 
    Person <$> nonEmpty name' <*> nonEmpty pl'

main :: IO ()
main = print "Hello!"
