from itertools import *

def to_infinity():
    i = 1
    while True:
        yield i
        i = i + 1

it = to_infinity()
res1 = list(islice(it, 3))
print(res1)

it2 = islice('abcdefg', 3, 5)
res2 = ''.join(it2)
print(res2)

it3 = islice("abcdefghijklmn", 0, None, 3)
res3 = ''.join(it3)
print(res3)

