module SquareNum where

squareNum :: [Integer] -> Integer
squareNum xs = sum $ map (\v -> v * v) xs
