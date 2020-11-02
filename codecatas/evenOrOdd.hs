evenOrOdd :: Integral a => a -> String
evenOrOdd n | even n = "Even"
            | otherwise = "Odd"
