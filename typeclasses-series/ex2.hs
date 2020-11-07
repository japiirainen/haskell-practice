module PreferJ where

preferJ :: Foldable a => a Char -> a Char -> a Char
preferJ x y = if 'j' `elem` x then x else y
