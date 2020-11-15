module StupidMath where


fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fact :: (Integral a) => a -> a
fact x = product [1 .. x]
