module File where

import           Control.Exception

main :: IO ()
main = do
    let file = "filess.txt"
    content <- try (readFile file) :: IO (Either SomeException String)
    case content of
        Left err -> putStrLn $ "caucht exeption => " <> show err
        Right c  -> putStrLn c
