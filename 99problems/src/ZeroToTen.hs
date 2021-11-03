{-# LANGUAGE DeriveAnyClass #-}
module ZeroToTen where

import qualified Data.List as List
import           Prelude   hiding (last, length, reverse)
import qualified Prelude

{-|
No.1
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
No.2
>>> butLast [1, 2, 3, 4]
>>> butLast ['a' .. 'z']
>>> butLast []
Just 3
Just 'y'
Nothing
-}
butLast :: [a] -> Maybe a
butLast xs = case (take 2 . Prelude.reverse) xs of
    (x:y:_) -> Just y
    _       -> Nothing


{-|
No.3
>>> elementAt [1, 2, 3] 2
>>> elementAt "haskell" 5
Just 2
Just 'e'
-}
elementAt :: [a] -> Int -> Maybe a
elementAt xs n =
    if n > Prelude.length xs
        then Nothing
        else Just $ (fst . Prelude.last) $ zip xs [1 .. n]

{-|
No.4
>>> length [1, 2, 3]
>>> length "haskell"
-}
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

{-|
No.5
>>> reverse [1, 2, 3]
>>> reverse "haskell"
[3,2,1]
"lleksah"
-}
reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

{-|
No.6
>>> isPalindrome "madamimadam"
>>> isPalindrome [1, 2, 3, 2, 1]
>>> isPalindrome [1, 2, 3]
True
True
False
-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

{-|
No.7
>>> flatten (Elem 5)
>>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
>>> flatten (List [])
[5]
[1,2,3,4,5]
[]
-}
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = mconcat $ map flatten xs

flatten' :: NestedList a -> [a]
flatten' (Elem x)  = return x
flatten' (List xs) = flatten' =<< xs

{-|
No.8
>>> compress "aaaabccaadeeee"
>>> compress [1,2,2,2,3,3,3,4]
"abcade"
[1,2,3,4]
-}
compress :: Eq a => [a] -> [a]
compress = (<$>) List.head . List.group


{-|
No.9
>>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
-}
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:same) : pack rest
    where (same, rest) = span (==x) xs

{-|
No.10
>>> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

