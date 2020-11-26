module ApplicativeMap where

import Data.Map (Map)
import qualified Data.Map as Map

data a >> b = Constant b | Table (Map a b)
    deriving (Eq, Show, Functor)

lookup' :: Ord a => a -> (a >> b) -> Maybe b
lookup' _ (Constant x) = Just x
lookup' k (Table m) = Map.lookup k m

tweet :: String -> String -> String -> String
tweet handle name location =
    handle <> " Hello " <> name <> ", " <>
    "we have an exciting opportunity " <>
    "in " <> location <> "."

names :: Integer >> String
names = Table $ Map.fromList
    [ (1, "Julie")
    , (3, "Chris")
    , (4, "Alonzo")
    ]

handles :: Integer >> String
handles = Table $ Map.fromList
    [ (1, "@argumatronic")
    , (2, "@typeclasses")
    , (3, "@chris__martin")
    ]

locations :: Integer >> String
locations = Constant "the Bay Area"

tweets :: Integer >> String
tweets = pure tweet <*> handles <*> names <*> locations

instance Ord a => Applicative ((>>) a)
  where
    pure = Constant
    liftA2 f (Table m1) (Table m2) = Table (Map.intersectionWith f m1 m2)
    liftA2 f (Constant x) (Table m2) = Table (Map.map (\y -> f x y) m2)
    liftA2 f (Table m1) (Constant y) = Table (Map.map (\x -> f x y) m1)
    liftA2 f (Constant x) (Constant y) = Constant (f x y)
