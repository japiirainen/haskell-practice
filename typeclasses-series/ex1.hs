friendsAge :: String -> Integer
friendsAge s =
    case s of
    "Allu" -> 22
    "jeppe" -> 33
    _ -> 100

main :: IO ()
main = putStrLn "hello"
