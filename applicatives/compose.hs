{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
module Compose where

import Control.Applicative

newtype ReaderIO env x = ReaderIO (env -> IO x)
    deriving Functor
    deriving Applicative via Compose ((->) env) IO

composePure :: (Applicative f1, Applicative f2) =>
               a -> f1 (f2 a)
composePure = pure . pure

composeLiftA2 :: (Applicative f1, Applicative f2) =>
                 (a -> b -> c) -> f1 (f2 a)
                               -> f1 (f2 b)
                               -> f1 (f2 c)
composeLiftA2 f = liftA2 (liftA2 f) 


compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (composePure x)
    liftA2 f (Compose x) (Compose y) =
        Compose (composeLiftA2 f x y)
    Compose x <*> Compose y = Compose (liftA2 (<*>) x y)

newtype MaybeList a = MaybeList (Maybe [a])
    deriving (Functor, Show)
    deriving Applicative via Compose Maybe []

main :: IO ()
main = print "hello"
