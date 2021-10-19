module Lib
    ( someFunc
    ) where
import           Control.Applicative (Applicative (liftA2))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Identity a = I a

instance Functor Identity where
    fmap f (I a) = I $ f a


instance Applicative Identity where
    liftA2 f (I a) (I b) = I $ f a b
    (I a) <*> b = fmap a b
    pure = I

instance Monad Identity where
    return = I
    (I a) >>= f = f a
