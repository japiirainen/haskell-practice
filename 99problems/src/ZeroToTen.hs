module ZeroToTen where

import           Prelude hiding (last)

{-|
>>> last [1, 2, 3]
>>> last []
Just 3
Nothing
-}
last :: [a] -> Maybe a
last []     = Nothing
last [x]    = Just x
last (_:xs) = last xs


{-|
>>> butLast [1, 2, 3, 4]
>>> butLast ['a' .. 'z']
>>> butLast []
Just 3
Just 'y'
Nothing
-}
butLast :: [a] -> Maybe a
butLast xs = case (take 2 . reverse) xs of
    (x:y:_) -> Just y
    _       -> Nothing

