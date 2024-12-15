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

# v = math.Vec2(3+9j,4)
# print(repr(v))
# print(v==v)
# v[0]=5
# print(v[1,0])
# v.x=1
# v1=math.Vec2(2,4)
# z=math.Vec3(3,4,4)
# print(v in math.Sets.Q2)
# print(v.x)
# print(repr(v))
# print(v==math.Vec2(2,4))
# print(complex(v))
# print(len(v1))
# for e in reversed(v):
#     print(e)
# print(v+v1)
# print(2*v)
# print(+(2*v)/2)
# v+=v1
# print(v)
# print(v.len)

# a = math.Matrix[2,3](0,-6,5,-1,2,-1)
# b = math.Matrix[3,2](-3,-5,-5,-1,-1,-3)
# c = math.Matrix[2,3](0,-1,2,1,-2,1)
# print(a*b*c) # should give 'Matrix[2, 3](-9, -7, 41, 6, -6, -6)'

# print(a.t)
# print(math.sqrt((v.t.conjugate()*v)[0]))
# print(v.y)
# print(a[0])
# print(v.g)

# print(v)
# print(v.map(lambda z:2*z))

# Mat1x1 = math.Matrix[1,1]
# m = Mat1x1(2.3+2j)

# m = math.Matrix[3,4](-1,1,0,1,-2,-3,-1,-2,-3,-1,-2,-1)
# print(m.RREF)
# print(m.RREF)
# m[0,0]=0
# print(m.RREF)

a = math.Matrix[2,2](3,4,2,-5)
print(a.diag)
print(a.RREF)
print(a.det)
print(a.inv)
print(a*a.inv)

# import os

# print(os.getcwd())

# file = io.File("Python\sadbox.py")
# print(file.exists())
# print(file.readStr())