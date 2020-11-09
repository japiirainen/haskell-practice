{-# LANGUAGE InstanceSigs #-}

module Laws where

--flipped Either

data FlippedEither b a = Error a | Success b deriving Show


instance Functor (FlippedEither s) where
    fmap :: (a -> b) -> FlippedEither s a -> FlippedEither s b
    fmap f (Success y) = Success y
    fmap f (Error y)   = Error (f y)

--exercise: Write a re-ordered tuple functor,
--such that fmap (+1) (3, 4) returns (4,4) rather than (3,5)

--instance Functor ((,) b) where
--    fmap f (x, y) = (f x, y)

main :: IO ()
main = putStrLn "Hello!"
