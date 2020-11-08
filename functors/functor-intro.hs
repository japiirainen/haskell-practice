module FunctorIntro where

database :: [(Integer, String)]
database = [(1, "Joona")
           , (2, "Chris")
           , (3, "Alonzo")
           , (4, "Melman")]

greetUser :: Integer -> Maybe String
greetUser x =
      ("Hello, " ++) <$> lookup x database
