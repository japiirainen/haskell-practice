module CommonFunctors where

import qualified Data.Text as T


database :: [(Integer, String)]
database = [(1, "Julie"),
            (2, "Chris"),
            (3, "Alonzo"),
            (4, "Melman")]

newDatabase :: [(Integer, T.Text)]
newDatabase = cleanupDB database

convertToText :: (Integer, String) -> (Integer, T.Text)
convertToText xs = T.pack <$> xs

convertDB :: [(Integer, String)] -> [(Integer, T.Text)]
convertDB xs = convertToText <$> xs

cleanupDB :: [(Integer, String)] -> [(Integer, T.Text)]
cleanupDB xs = (fmap . fmap) T.strip (convertDB xs)

greetUser :: Integer -> String
greetUser x =
    fmap ("Hello, " ++) T.unpack username
        where username = 
                case lookup x newDatabase of
                    Just a -> a
                    Nothing -> T.pack "no user found with that id"


data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair l a) = Pair (f l) (f a)

data Username a = Username a a deriving Show
instance Functor Username where
    fmap f (Username l a) = Username (f l) (f a)

user :: Username String
user = Username " Julie " "  Modonuki"

users :: [Username String]
users = [Username " Julie " "Moronuki", Username "Chris " " Martin"]

stripSpaces :: Username String -> Username String
stripSpaces username = fmap (T.unpack . T.strip . T.pack) username

stripUsers :: [Username String] -> [Username String]
stripUsers = Prelude.map stripSpaces

instance Functor ((,,) a b) where
    fmap f (x, y, a) = (x, y, f a)


main :: IO ()
main = undefined
