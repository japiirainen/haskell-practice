{-# LANGUAGE InstanceSigs #-}

module Laws where

--flipped Either

data FlippedEither b a = Error a | Success b deriving Show


instance Functor (FlippedEither s) where
    fmap :: (a -> b) -> FlippedEither s a -> FlippedEither s b
    fmap f (Success y) = Success y
    fmap f (Error y)   = Error (f y)

--instance Functor ((->) a) where
--    fmap :: (a -> b) -> ((->) w) a -> ((->) w) b
--    fmap = (.)

main :: IO ()
main = putStrLn "Hello!"
