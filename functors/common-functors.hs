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

main :: IO ()
main = undefined
