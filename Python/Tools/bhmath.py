from Tools.log import log, Severity
import Tools.constants as Constants

from math import sqrt
from enum import IntEnum
from typing import Union, Iterable, Callable, Iterator, Any, Self
from numbers import Complex, Real, Rational, Integral

class SetTypes(IntEnum):
	Empty=0
	All=1
     
class Set:
    class _Generator:
        class _Internal:
            def __init__(self, gen, conditions):
                self.iter = iter(gen)
                self.conditions = conditions
                self.counter = set([])
            def _meetsConditions(self, x):
                return set([cond(x) for cond in self.conditions])=={True}
            def __iter__(self):
                return self
            def __next__(self):
                while not self._meetsConditions(x:=self.iter.__next__()) or x in self.counter: pass
                self.counter.add(x)
                return x

        def __init__(self, gen, conditions):
            self.gen = gen
            self.conditions = conditions
        def __call__(self):
            return self.__iter__()
        def __iter__(self):
            return self._Internal(iter(self.gen), self.conditions)

    class _NotIterable:
        def __init__(self, msg):
            self.msg = msg
        def __call__(self):
            return self
        def __iter__(self):
            return self
        def __next__(self)->None:
            log(self.msg, Severity.crash)
            raise StopIteration()

    def _meetsConditions(self, x):
        for cond in self._conditions:
            try:
                v = cond(x)
                if not v: return False
            except: return False
        return True

    def __init__(self, base:Union[SetTypes, set, list, Iterable], conditions:Iterable[Callable[[Any], bool]] = [lambda x:True])->None:
        """Creates a new Set object

        args:
            base (set, list or Iterable): When type is set, Set or list, the Set will contain the (finite) number of items in the base with the conditions applied. When type is Iterable, the set is assumed infite, but any item can be generated by the generator, either sequentially or by index. When type is Sets, the base will be the specified enum value.
            conditions (function): Specifies extra conditions on the base that must be met for an item to be in the new Set
        """

        if type(base) == SetTypes:
            if base == SetTypes.Empty:
                self._finite = {}
                self._conditions = []
                self._generator = self._finite.__iter__
            elif base == SetTypes.All:
                self._finite = SetTypes.All
                self._conditions = conditions
                self._generator = self._NotIterable("This Set is infinite and provides no generator.")
            else:
                log("Unkown Sets enum!", Severity.crash)
        elif type(base) == set or type(base) == list:
            self._conditions = conditions
            self._finite = set([x for x in base if self._meetsConditions(x)]) # filter out all elements of base that do not meet the conditions criteria
            self._conditions = [] # after all conditions are applied to the _finite internal set, they serve no more purpose
            self._generator = self._finite.__iter__
        elif isinstance(base, Iterable):
            self._finite = None
            self._conditions = conditions
            self._generator = self._Generator(base, conditions)
        else:
            log("Unsupported type for Set base!", Severity.crash)

    def contains(self, x):
        if self._finite == SetTypes.All:
            return self._meetsConditions(x)
        if self._finite != None:
            return x in self._finite
        # We are dealing with a set made up of conditions and a generator
        if not self._meetsConditions(x):
            return False
        return x in self._generator()

    def __contains__(self, x):
        return self.contains(x)

    def __str__(self):
        if self._finite == SetTypes.All:
            return "Ω with conditions"
        if self._finite != None:
            return str(self._finite)
        # We are dealing with a set made up of conditions and a generator
        return str(set([x for x in self._generator() if self._meetsConditions(x)]))
    
    def __iter__(self)->Iterator:
        return self._generator()

class Matrix:

    _class_cache = {} # Stores previously created classes according to their dimension
    _identity_cache = {} # Stores previously created identity matrices according to their dimension

    def __class_getitem__(cls, dims:tuple[int, int])->type:
        # Check that dims is a tuple of type (dim1, dim2)
        if not isinstance(dims, tuple): raise TypeError(f"Expected a tuple to specify the dimensions, but got {type(dims)}.")
        if len(dims) != 2: raise ValueError(f"Expected 2 dimension parameters, but got {len(dims)}.")
        if not isinstance(dims[0], int) or not isinstance(dims[1], int): raise TypeError(f"Expected dimensions to be of type (int, int), but got ({type(dims[0])}, {type(dims[1])}).")

        if dims in cls._class_cache: return cls._class_cache[dims] # If the class for this dimension was already created, return that one

        class MatWithDim:
            def __init__(self, *args: Iterable[Complex], checkValidity:bool=True)->None:
                self.dims = dims # Should be a tuple like (dim1, dim2)
                self.args = list(args) # assume row1, row2, ... ordering

                # Checking validity
                if checkValidity:
                    if len(self.args) != self.dims[0] * self.dims[1]: raise ValueError(f"Recieved incorrect number of arguments for {self.dims[0]}x{self.dims[1]} Matrix. Expected {self.dims[0] * self.dims[1]}, but got {len(self.args)}.")
                    for i, e in enumerate(self.args):
                        if not isinstance(e, Complex): raise TypeError(f"Element at index {i} is of type {type(e)}, but expected Complex.")

                # Setting up cached values
                self._cached_RREF = None
                self._cached_det = None
                self._cached_inv = None
                self._cacheValidationKey = hash(self) # Remember: when adding cached values, clear them in _isCacheValid

            def _isCacheValid(self):
                res = self._cacheValidationKey == hash(self)
                if res != True:
                    self._cached_RREF = None
                    self._cached_det = None
                    self._cached_inv = None
                return res

            @property
            def x(self)->Complex: return self.args[0]
            @x.setter
            def x(self, value:Complex): self.args[0] = value
            @property
            def y(self)->Complex: return self.args[1]
            @y.setter
            def y(self, value:Complex): self.args[1] = value
            @property
            def z(self)->Complex: return self.args[2]
            @z.setter
            def z(self, value:Complex): self.args[2] = value
            @property
            def w(self)->Complex: return self.args[3]
            @w.setter
            def w(self, value:Complex): self.args[3] = value

            @property
            def r(self)->Complex: return self.args[0]
            @r.setter
            def r(self, value:Complex): self.args[0] = value
            @property
            def g(self)->Complex: return self.args[1]
            @g.setter
            def g(self, value:Complex): self.args[1] = value
            @property
            def b(self)->Complex: return self.args[2]
            @b.setter
            def b(self, value:Complex): self.args[2] = value
            @property
            def a(self)->Complex: return self.args[3]
            @a.setter
            def a(self, value:Complex): self.args[3] = value

            @property
            def len(self)->Real: return sqrt(sum([z*z.conjugate() for z in self.args]))
            @property
            def t(self)->Self: return Matrix[self.dims[1], self.dims[0]](*[self[i,j] for j in range(self.dims[1]) for i in range(self.dims[0])], checkValidity=False)
            
            @property
            def RREF(self) -> Self:
                """Returns the Reduced Row Echelon Form of the Matrix"""

                if self._cached_RREF != None and self._isCacheValid():
                    return self._cached_RREF

                det = [1]
                def findNonZeroIndexInColumn(mat, c):
                    for i in range(c, mat.dims[0]):
                        if mat[i,c] != 0: return i
                    return -1
                def swapColumns(mat, c1, c2, det=det):
                    col1 = [mat[i, c1] for i in range(mat.dims[0])]
                    col2 = [mat[i, c2] for i in range(mat.dims[0])]
                    res = mat
                    for i in range(res.dims[0]):
                        res[i, c1] = col2[i]
                        res[i, c2] = col1[i]
                    if c1 != c2:
                        det[0] *= -1
                    return res
                def swapRows(mat, r1, r2, det=det):
                    col1 = [mat[r1, j] for j in range(mat.dims[1])]
                    col2 = [mat[r2, j] for j in range(mat.dims[1])]
                    res = mat
                    for j in range(res.dims[1]):
                        res[r1, j] = col2[j]
                        res[r2, j] = col1[j]
                    if r1 != r2:
                        det[0] *= -1
                    return res
                def divideRow(mat, r, v, det=det):
                    res = mat
                    for j in range(res.dims[1]):
                        res[r,j] /= v
                    det[0] *= v
                    return res
                def LCRows(mat, a, r1, r2): # r2+=a*r1
                    res = mat
                    for j in range(res.dims[1]):
                        res[r2,j] += a * res[r1, j]
                    return res

                res = Matrix[self.dims](*self.args, checkValidity=False)
                zeroColsCount = 0 # Number of confirmed columns at the end of the matrix that are 0
                for c in range(min(self.dims)):
                    # Make the current column non-zero
                    while (r:= findNonZeroIndexInColumn(res, c)) == -1:
                        if c + zeroColsCount >= dims[1]:break # This corresponds to the back columns being filled with zeros up to the current working column, meaning the algorithm has reached its end
                        zeroColsCount += 1
                        res = swapColumns(res, c, -zeroColsCount)
                    if c + zeroColsCount >= dims[1]:break # This corresponds to the back columns being filled with zeros up to the current working column, meaning the algorithm has reached its end

                    # Bring the row with non-zero element at [r, c] to [c, c]
                    res = swapRows(res, r, c)
                    # Make the first element 1
                    res = divideRow(res, c, res[c,c])

                    # Make everything above and below zero in that column
                    for i in range(res.dims[0]):
                        if i==c: continue
                        res = LCRows(res, -res[i,c], c, i)

                # Finish calculating the determinant:
                for i in range(min(self.dims)):
                    det[0] *= res[i,i]

                self._cached_RREF = res
                self._cached_det = det[0]
                return res

            @property
            def diag(self) -> list: return [self[i,i] for i in range(self.dims[0])]
            @property
            def det(self) -> Complex:
                if self._cached_det != None and self._isCacheValid():
                    return self._cached_det
                
                self._cached_RREF = None
                self.RREF # Calculates and stores determinant as well
                return self._cached_det
            @property
            def inv(self) -> Self:
                if self._cached_inv != None and self._isCacheValid():
                    return self._cached_inv
                
                id = Matrix.Id(self.dims[0])
                extendedRREF = (self | id).RREF
                right = Matrix[self.dims](*[extendedRREF[r,c+self.dims[1]] for r in range(self.dims[0]) for c in range(self.dims[1])])
                
                if Matrix.isZeroMat(right*self-id): # Inverse succeeded
                    self._cached_inv = right
                    return right
                self._cached_inv = None
                self._cached_det = 0
                raise ZeroDivisionError(f"Cannot calculate inverse as the determinant is 0.")

            def __repr__(self) -> str:
                return f"Matrix[{self.dims[0]}, {self.dims[1]}]({', '.join([repr(x) for x in self.args])})"
            def __eq__(self, value: object) -> bool:
                if not isinstance(value, Matrix[self.dims]): return False
                if set([value.args[i]==self.args[i] for i in range(len(self.args))]) != {True}: return False
                return True
            def __hash__(self) -> int:
                return hash(tuple(self.args))
            def __bool__(self) -> bool:
                return set([x==0 for x in self.args]) != {False}
            def __complex__(self) -> complex:
                if self.dims != (2,1): return NotImplemented
                return complex(self.x, self.y)
            def __len__(self) -> int:
                return self.dims[0]*self.dims[1]
            def __iter__(self) -> Iterator:
                return iter(self.args)
            def __getitem__(self, key:int) -> complex:
                if isinstance(key, Iterable): # mat[i, j]
                    if len(key) != 2: return NotImplemented
                    if (i:=key[0]) < 0: i+=self.dims[0]
                    if i < 0 or i >= self.dims[0]: raise IndexError("Index out of range")
                    if (j:=key[1]) < 0: j+=self.dims[1]
                    if j < 0 or j >= self.dims[1]: raise IndexError("Index out of range")
                    return self.args[j + i*self.dims[1]]
                if isinstance(key, Integral): # vec[i]
                    return self.args[key]
                return NotImplemented
            def __setitem__(self, key:int, value:complex) -> None:
                if not isinstance(value, Complex): raise TypeError(f"Expected value to be of type Complex, but got {type(value)}.")
                if isinstance(key, Iterable): # mat[i, j]
                    if len(key) != 2: return NotImplemented
                    if (i:=key[0]) < 0: i+=self.dims[0]
                    if i < 0 or i >= self.dims[0]: raise IndexError("Index out of range")
                    if (j:=key[1]) < 0: j+=self.dims[1]
                    if j < 0 or j >= self.dims[1]: raise IndexError("Index out of range")
                    self.args[j + i*self.dims[1]] = value
                if isinstance(key, Integral): # vec[i]
                    self.args[key] = value
                return NotImplemented
            def __contains__(self, value:object) -> bool:
                return value in self.args
            def __add__(self, y:object) -> Self:
                if not isinstance(y, Matrix[self.dims]): return NotImplemented
                return Matrix[self.dims](*[x+y[i] for i, x in enumerate(self.args)], checkValidity=False)
            def __sub__(self, y:object) -> Self:
                if not isinstance(y, Matrix[self.dims]): return NotImplemented
                return Matrix[self.dims](*[x-y[i] for i, x in enumerate(self.args)], checkValidity=False)
            def __mul__(self, y:object) -> Self: # x*y
                if isinstance(y, Complex):
                    return Matrix[self.dims](*[x*y for x in self.args], checkValidity=False)
                if type(y) in cls._class_cache.values():
                    if self.dims[1] != y.dims[0]: return NotImplemented
                    return Matrix[self.dims[0],y.dims[1]](*[sum(self[i, k]*y[k, j] for k in range(self.dims[1])) for i in range(self.dims[0]) for j in range(y.dims[1])]) # I could set checkValidity=False, but I don't trust myself
                return NotImplemented
            def __rmul__(self, y:object) -> Self: # y*x
                if not isinstance(y, Complex): return NotImplemented # the case where y is another matrix is covered by that y's __mul__ implementation
                return Matrix[self.dims](*[x*y for x in self.args], checkValidity=False)
            def __truediv__(self, y:object) -> Self:
                if not isinstance(y, Complex): return NotImplemented
                return 1/y * self
            def __neg__(self) -> Self:
                return Matrix[self.dims](*[-x for x in self.args], checkValidity=False)
            def __pos__(self) -> Self:
                return Matrix[self.dims](*self.args, checkValidity=False)
            def __or__(self, y:object) -> Self:
                if type(y) in cls._class_cache.values():
                    if y.dims[0] != self.dims[0]: return NotImplemented
                    return Matrix[self.dims[0], self.dims[1]+y.dims[1]](*sum([self.args[r*self.dims[1]: (r+1)*self.dims[1]] + y.args[r*y.dims[1]: (r+1)*y.dims[1]] for r in range(self.dims[0])], []))
                return NotImplemented

            def conjugate(self) -> Self:
                return Matrix[self.dims](*[x.conjugate() for x in self.args], checkValidity=False)
            def map(self, f:Callable[[Complex],Complex]) -> Self:
                return Matrix[self.dims](*[f(x) for x in self.args])

        # Finishing up the class by setting its name and deleting unsensical attributes
        MatWithDim.__name__ = f"Matrix[{dims[0]}, {dims[1]}]"
        if dims[1] != 1:
            del MatWithDim.x
            del MatWithDim.y
            del MatWithDim.z
            del MatWithDim.w
            del MatWithDim.r
            del MatWithDim.g
            del MatWithDim.b
            del MatWithDim.a
        
        if dims[0] != dims[1]:
            del MatWithDim.diag
            del MatWithDim.det
            del MatWithDim.inv
        
        cls._class_cache[dims] = MatWithDim
        return MatWithDim

    @classmethod
    def Id(cls, n:int) -> Self:
        if not isinstance(n, Integral): raise TypeError(f"Expected an int to specify the dimensions, but got {type(n)}.")
        if n < 1: raise ValueError(f"Dimension of an nxn matrix cannot be less than 1, but got {n}")

        if n in cls._identity_cache: return cls._identity_cache[n] # If the identity matrix for this dimension was already created, return that one
        
        cls._class_cache[n] = Matrix[n,n](*[1 if i-j == 0 else 0 for i in range(n) for j in range(n)])
        return cls._class_cache[n]

    @classmethod
    def isZeroMat(cls, y:Self) -> bool:
        if not type(y) in cls._class_cache.values(): return False
        for item in y.args:
            if abs(item) > Constants.floatEpsilon:
                return False
        return True

Vec2 = Matrix[2,1]
Vec3 = Matrix[3,1]
Vec4 = Matrix[4,1]

Mat2 = Matrix[2,2]
Mat3 = Matrix[3,3]
Mat4 = Matrix[4,4]

Id2 = Matrix.Id(2)
Id3 = Matrix.Id(3)
Id4 = Matrix.Id(4)

class Sets:
    C = Set(SetTypes.All, [lambda x:isinstance(x, Complex)])
    R = Set(SetTypes.All, [lambda x:isinstance(x, Real)])
    Q = Set(SetTypes.All, [lambda x:isinstance(x, Rational)]) # any Real number that has numerator and denominator properties
    Z = Set(SetTypes.All, [lambda x:isinstance(x, Integral)])
    N = Set(SetTypes.All, [lambda x:(x in Sets.Z) and x >= 0]) # includes 0

    C2 = Set(SetTypes.All, [lambda v:isinstance(v, Vec2)])
    R2 = Set(SetTypes.All, [lambda v:v in Sets.C2] + [lambda v: v[i] in Sets.R for i in range(2)])
    Q2 = Set(SetTypes.All, [lambda v:v in Sets.C2] + [lambda v: v[i] in Sets.Q for i in range(2)])

    C3 = Set(SetTypes.All, [lambda v:isinstance(v, Vec3)])
    R3 = Set(SetTypes.All, [lambda v:v in Sets.C3] + [lambda v: v[i] in Sets.R for i in range(3)])
    Q3 = Set(SetTypes.All, [lambda v:v in Sets.C3] + [lambda v: v[i] in Sets.Q for i in range(3)])
    
    C4 = Set(SetTypes.All, [lambda v:isinstance(v, Vec4)])
    R4 = Set(SetTypes.All, [lambda v:v in Sets.C4] + [lambda v: v[i] in Sets.R for i in range(4)])
    Q4 = Set(SetTypes.All, [lambda v:v in Sets.C4] + [lambda v: v[i] in Sets.Q for i in range(4)])