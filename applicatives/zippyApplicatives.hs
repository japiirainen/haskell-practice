module Zippy where


main :: IO ()
main =
    do
      putStrLn "Tell me your name!?"
      name <- getLine
      putStrLn $ "Well hello " <> name <> "!"
