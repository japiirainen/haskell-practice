module ApplicativeCompose where

main :: IO ()
main = do
    print $ sum nums
    where
        nums :: (Num a) => [a]
        nums = [1, 2, 3, 4]