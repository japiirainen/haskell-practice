module BasicTypes2 where

idk :: (Ord a, Num a) => a -> a
idk x = if (x < 10) then (negate x) else (x * 10)


ikd2 :: (Ord a, Num a) => a -> a
ikd2 x = 
    case x < 10 of
        True -> negate x
        False -> x * 10


-- Some basic types in haskell

--data Bool    = True    | False
--data Maybe a = Nothing | Just a
--data [] a    = []      | a : [a]
