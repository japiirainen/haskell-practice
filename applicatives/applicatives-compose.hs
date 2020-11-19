{-# LANGUAGE InstanceSigs #-}
module ApplicativeCompose where

import Data.List
import Data.Char
import Control.Applicative

weird :: IO Char
weird = fmap (const 1) (putStrLn "hello") *> fmap (const 'a') (putStrLn "world")

weird2 :: IO Char
weird2 = fmap (const 1) (putStrLn "hello") *> fmap (const 'a') (putStrLn "world")

alphabetize :: [Char] -> [Char] -> [[Char]]
alphabetize name1 name2 = sort [name1, name2]

alphabetizeIO :: IO [String]
alphabetizeIO = pure alphabetize
    <*> getLine <*> getLine

alphabetizeMaybe :: String -> String -> Maybe [String]
alphabetizeMaybe x y =
    case all isAlpha x && all isAlpha y of
      True -> Just $ alphabetize x y
      False -> Nothing

alphabetizeMaybeIO :: IO (Maybe [String])
alphabetizeMaybeIO = pure alphabetizeMaybe
    <*> getLine <*> getLine

-- to get to the middle of these functors with fmap
sucks :: IO (Maybe [String])
sucks = (fmap . fmap . fmap . fmap) toUpper $ alphabetizeMaybeIO

newtype MaybeList a = MaybeList (Maybe [a])
    deriving (Show)

instance Functor MaybeList where
    fmap f (MaybeList xs) = MaybeList (fmap (fmap f) xs)

-- for completeness both liftA2 and (<*>)
instance Applicative MaybeList where
    pure a = MaybeList (Just [a])
    liftA2 f (MaybeList x) (MaybeList y) = MaybeList (liftA2 (liftA2 f) x y)
    (<*>) ___ (MaybeList Nothing) = MaybeList Nothing
    (<*>) (MaybeList Nothing) ___ = MaybeList Nothing
    (<*>) (MaybeList (Just xs)) (MaybeList (Just ys)) =
        MaybeList (Just (xs <*> ys))

newtype ReaderIO env a = ReaderIO (env -> IO a)

runReaderIO :: ReaderIO env a -> env -> IO a
runReaderIO (ReaderIO f) = f

instance Functor (ReaderIO env) where
    fmap :: (a -> b) -> ReaderIO env a
                     -> ReaderIO env b
    fmap f (ReaderIO g) = ReaderIO $ \env -> fmap f (g env)

instance Applicative (ReaderIO env) where
    pure :: a -> ReaderIO env a
    pure x = ReaderIO $ \_env -> pure x
    liftA2 :: (a -> b -> c) 
           -> ReaderIO env a
           -> ReaderIO env b
           -> ReaderIO env c
    liftA2 f (ReaderIO g) (ReaderIO h) = 
        ReaderIO $ \env ->
            pure f <*> g env <*> h env

data Order = Alphabetical | Forward | Reverse

arrange :: Order -> String -> String -> [String]
arrange Forward x y = [x, y]
arrange Reverse x y = [y, x]
arrange Alphabetical x y = sort [x, y]

data LineLimit = NoLimit | MaxLength Int

applyLineLimit :: LineLimit -> String -> String
applyLineLimit NoLimit x = x
applyLineLimit (MaxLength n) x = take n x

data Config = 
    Config { configOrder :: Order,
             configLineLimit :: LineLimit }

arrange' :: Config -> String -> String -> [String]
arrange' config = arrange (configOrder config)

getLine' :: Config -> IO String
getLine' config =
    applyLineLimit (configLineLimit config)
        <$> getLine

fn :: (env -> a) -> ReaderIO env a
fn f = ReaderIO (\env -> pure (f env))

getAndArrange' :: Config -> IO [String]
getAndArrange' = runReaderIO 
    (fn arrange' <*> ReaderIO getLine' <*> ReaderIO getLine')

newtype ReaderT env f a = ReaderT (env -> f a)

instance (Functor f) => Functor (ReaderT env f) where
    fmap f (ReaderT xs) = ReaderT (fmap (fmap f) xs)

instance (Functor f, Applicative f) => Applicative (ReaderT env f) where
    pure x = ReaderT (pure (pure x))
    liftA2 f (ReaderT x) (ReaderT y) = ReaderT (liftA2 (liftA2 f) x y)

main = do
    putStrLn "Hello" *> putStrLn "world"

