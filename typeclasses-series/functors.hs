module Functors where


database :: [(Integer, String)]
database = [(1, "Julie"),
            (2, "Jonotte"),
            (3, "Manakine"),
            (4, "Kanakkune")]

greetUser :: Integer -> Either String String
greetUser record = fmap ("Hello, " ++) user
    where user = 
            case lookup record database of
                Just x -> Right x
                Nothing -> Left "no user found :-("



mapToMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapToMaybe f Nothing = Nothing
mapToMaybe f (Just x) = Just (f x)

mapToEither :: (a -> b) -> Either left a -> Either left b
mapToEither f (Left x) = Left x
mapToEither f (Right x) = Right (f x)


--class Functor (f :: * -> *) where
--    fmap :: (a -> b) -> f a -> f b
