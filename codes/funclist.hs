import Data.Maybe (fromJust)

f1 x = 2.4*x
f2 x = 3.5*x -10
f3 x = 4*x^^2 - 10*x + 100
dict = [('a', f1), ('b', f2), ('c', f3)]


funcdict key value = (fromJust $ lookup key dict) value

funcdict2 'a' x = f1 x
funcdict2 'b' x = f2 x
funcdict2 'c' x = f3 x
