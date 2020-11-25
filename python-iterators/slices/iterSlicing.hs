module IterSlicing where

toInfinity :: [Int]
toInfinity = enumFrom 1

some :: String
some = take 2 (drop 3 "abcdefg")

it :: (Eq t, Num t) => t -> t -> [a] -> [a]
it _ _ [] = []
it 0 step (x:xs) = x : it (step - 1) step xs
it start step (x:xs) = it (start - 1) step xs

main :: IO ()
main = do
    print $ take 3 toInfinity
    print some
    print $ it 0 3 "abcdefghijklmn"
