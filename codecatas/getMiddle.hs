module GetMiddle where

getMiddle :: String -> String
getMiddle "" = ""
getMiddle [a] = [a]
getMiddle [a, b] = [a, b]
getMiddle (_:xs) = getMiddle (init xs)
