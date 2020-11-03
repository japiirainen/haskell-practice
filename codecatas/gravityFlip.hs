module GrarivityFlip where

import Data.List

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip c xs
    | c == 'R' = sort xs
    | otherwise = reverse $ sort xs

