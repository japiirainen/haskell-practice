module BreakingLaws where

data Option a = Nope | Yep a deriving (Eq, Show)

instance Functor Option where
    fmap _ _ = Nope

data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair l r) = Pair (f l) (f r)

composed :: Functor f => f [a] -> f [a]
composed = fmap (reverse . take 5)

composedfmap :: Functor f => f [a] -> f [a]
composedfmap = fmap reverse . fmap (take 5)

test :: IO ()
test = do
    print (fmap id (Pair "Julie" "moronuki"))
    print (id (Pair "Julie" "moronuki"))
    print (composed (Pair "Julie" "moronuki"))
    print (composedfmap (Pair "Julie" "moronuki"))

data IncrementPair a =
    IncrementPair Integer a deriving (Show, Eq)

instance Functor IncrementPair where
    fmap f (IncrementPair int r) = IncrementPair (int + 1) (f r)

testIncPair :: IO ()
testIncPair = do
    print (id IncrementPair 5 5)
    print (fmap id $ IncrementPair 5 5)
    print (fmap ((*100) . (+1)) $ IncrementPair 5 5)
    print (fmap (*100) . fmap (+1) $ IncrementPair 5 5)

main :: IO ()
main = putStrLn "Well hello there!"
