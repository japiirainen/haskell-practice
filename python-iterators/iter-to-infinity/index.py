from itertools import *

it = count(2)
res = list(islice(it, 5))
print(res)

it2 = count(10, -1)
res2 = list(islice(it2, 5))
print(res2)

it3 = cycle("ha")
res3 = ''.join(islice(it3, 6))
print(res3)

it4 = repeat("ha")
res4 = list(islice(it4, 5))
print(res4)
