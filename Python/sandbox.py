import Tools.color as color
import Tools.io as io
import Tools.bhmath as math

# empty = math.Set(math.SetTypes.Empty)

# oneTwo = math.Set({1,2})
# condOneTwo = math.Set({1,2}, [lambda x: x%2==0])

# all = math.Set(math.SetTypes.All)
# allCond = math.Set(math.SetTypes.All, [lambda x: x%2==0])

# finGen = math.Set(range(10))
# finGenCond = math.Set(range(10), [lambda x: x%2==0])

# print(empty)
# for e in empty:
#     print(e)

# print("9+0.2j" in math.Sets.C)

v = math.Vec2(3,4)
v[0]=2
print(v in math.Sets.Q2)
print(v.x)


# import os

# print(os.getcwd())

# file = io.File("Python\sadbox.py")
# print(file.exists())
# print(file.readStr())