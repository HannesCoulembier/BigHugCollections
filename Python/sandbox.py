import Tools.color as color
import Tools.io as io
import Tools.bhmath as math

empty = math.Set(math.Sets.Empty)

oneTwo = math.Set({1,2})
condOneTwo = math.Set({1,2}, [lambda x: x%2==0])

all = math.Set(math.Sets.All)
allCond = math.Set(math.Sets.All, [lambda x: x%2==0])

finGen = math.Set(range(10))
finGenCond = math.Set(range(10), [lambda x: x%2==0])
for e in condOneTwo:
    print(e)
for e in condOneTwo:
    print(e)


# import os

# print(os.getcwd())

# file = io.File("Python\sadbox.py")
# print(file.exists())
# print(file.readStr())