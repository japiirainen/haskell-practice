module Index where

xs :: [Integer]
xs = iterate (* 2) 10

it :: [Integer]
it = iterate (\x -> x - 1) 10

ha :: String
ha = cycle "ha"

ha2 :: [String]
ha2 = repeat "ha"

main :: IO ()
main = do
    print $ take 5 xs
    print $ take 5 it
    print $ take 6 ha
    print $ take 5 ha2
    print $ replicate 5 "ha"
