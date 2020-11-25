import sys
import itertools

xs = itertools.count(1)

print(list(itertools.islice(xs, 10)))

def f():
    for _ in range(3):
        x = sys.stdin.readline()
        yield x.upper()

print(list(f()))


"""
same in hasell is:
import Data.Traversable (for)
import Data.Char (toUpper)

threeUppercase =
    for [1..3] $ \_ -> do
        x <- getLine
        return (map toUpper x)
"""
