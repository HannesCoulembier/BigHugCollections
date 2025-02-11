from Tools.bhlog import log, Severity
import Tools.bhconstants as Constants

from math import sqrt
from enum import IntEnum, Enum
from typing import Union, Iterable, Callable, Iterator, Any, Self
from numbers import Complex, Real, Rational, Integral
import inspect

class Result(Enum):
    FALSE=0
    TRUE=1
    UNSURE=2

    def __bool__(self):
        if self.value == 0: return False
        elif self.value == 1: return True
        else: raise NotImplementedError("UNSURE Result cannot be converted to boolean")
    def __str__(self) -> str:
        return str(self.name) 
    def __repr__(self) -> str:
        return self.__str__()  
    def __eq__(self, y:Any) -> bool:
        if type(y) == bool: return self == Result.TRUE if y else self == Result.FALSE
        if type(y) != Result: return NotImplemented
        return self.value == y.value
    def __or__(self, y:Any) -> Self:
        if type(y) == bool: return self | (Result.TRUE if y else Result.FALSE)
        if type(y) != Result: return NotImplemented
        if (self == Result.TRUE) or (y == Result.TRUE): return Result.TRUE
        if (self == Result.UNSURE) or (y == Result.UNSURE): return Result.UNSURE
        return Result.FALSE
    def __ror__(self, y:Any) -> Self:
        return self.__or__(y)
    def __and__(self, y:Any) -> Self:
        if type(y) == bool: return self & (Result.TRUE if y else Result.FALSE)
        if type(y) != Result: return NotImplemented
        if (self == Result.FALSE) or (y == Result.FALSE): return Result.FALSE
        if (self == Result.UNSURE) or (y == Result.UNSURE): return Result.UNSURE
        return Result.TRUE
    def __rand__(self, y:Any) -> Self:
        return self.__and__(y)

class Set:
    class _DescriptiveTypeIndicator: pass
    class _EmptyIndicator: pass
    Descriptive = _DescriptiveTypeIndicator()
    Empty = _EmptyIndicator()

    class Type(IntEnum):
        Finite=0
        Descriptive=1
        Union=2

    def _private_init_(self, type:Type, sureset:set, unsureset:set, unions:list[Self], conditions:list[Callable[[Any],Result]], parents:set[Self]):
        self.type = type

        self.sureset = sureset
        self.unsureset = unsureset
        self.unions = unions

        self.conditions = conditions

        self.parents = parents
        self.allParents = set(list(self.parents)+sum([list(parent.allParents) for parent in self.parents],[])) # A set of all parent Sets (usefull to determine if this Set "A" is a subset of another Set "B": If "B" is one of its parents, "A" is a subset)
        
    def _meetsConditions(self, x:Any) -> Result:
        allTrue = True
        for parent in self.parents:
            r = parent.contains(x)
            if r == Result.FALSE: return Result.FALSE
            if r == Result.UNSURE: allTrue = False
        for cond in self.conditions:
            try:
                r = cond(x)
            except:
                return Result.FALSE
            if r == Result.FALSE: return Result.FALSE
            if r == Result.UNSURE: allTrue = False
        if allTrue: return Result.TRUE
        return Result.UNSURE

    def __init__(self, base:Union[_DescriptiveTypeIndicator, Self, set, list], conditions:list[Callable[[Any],Result]]=[lambda x:Result.TRUE], parents:set=set()):

        if type(conditions) != list: raise TypeError("conditions should be a list")
        if set([isinstance(condition, Callable) for condition in conditions]) != {True}: raise TypeError("all conditions should be instances of Callable")
        if type(parents) != set: raise TypeError("parents should be a set")
        if parents != set():
            if set([type(parent) for parent in parents]) != {Set}: raise TypeError("All parents should be of type Set")

        if type(base) == Set._DescriptiveTypeIndicator:
            self._private_init_(Set.Type.Descriptive, set(), set(), [], conditions, parents)
        elif type(base) == Set._EmptyIndicator:
            self._private_init_(Set.Type.Finite, set(), set(), [], [], set())
        elif type(base) == list or type(base) == set:
            sureset = set()
            unsureset = set()
            for item in base:
                results = [cond(item) for cond in conditions] + [parent.contains(item) for parent in parents]
                if Result.FALSE in results: continue
                if Result.UNSURE in results: unsureset.add(item)
                else: sureset.add(item)
            self._private_init_(Set.Type.Finite, sureset, unsureset, [], [], parents)
        elif type(base) == Set:
            parents = parents.copy()
            parents.add(base)
            if base.type == Set.Type.Descriptive:
                self._private_init_(Set.Type.Descriptive, set(), set(), [], base.conditions+conditions, parents)
            elif base.type == Set.Type.Finite:
                sureset = set()
                unsureset = set()
                for item in base.sureset:
                    results = [cond(item) for cond in conditions] + [parent.contains(item) for parent in parents]
                    if Result.FALSE in results: continue
                    if Result.UNSURE in results: unsureset.add(item)
                    else: sureset.add(item)
                for item in base.unsureset:
                    results = [cond(item) for cond in conditions] + [parent.contains(item) for parent in parents]
                    if Result.FALSE in results: continue
                    unsureset.add(item)
                self._private_init_(Set.Type.Finite, sureset, unsureset, [], [], parents)
            elif base.type == Set.Type.Union:
                self._private_init_(Set.Type.Union, set(), set(), base.unions, base.conditions+conditions, parents)
            else:
                raise ValueError("Unknown Set.Type")
        else:
            raise TypeError("Unknown base type. Base should be Set.Descriptive, a Set, a set or list or an Iterable instance")

    def contains(self, x:Any) -> Result:

        if self.type == Set.Type.Finite:
            if x in self.sureset: return Result.TRUE
            if x in self.unsureset: return Result.UNSURE
            return Result.FALSE
        
        meetsCond = self._meetsConditions(x)
        if meetsCond == Result.FALSE: return Result.FALSE

        if self.type == Set.Type.Descriptive:
            return meetsCond
        elif self.type == Set.Type.Union:
            isFalse = True
            for union in self.unions:
                r = union.contains(x)
                if r == Result.TRUE: return meetsCond
                if r == Result.UNSURE: isFalse = False
            if isFalse: return Result.FALSE
            return Result.UNSURE
        else:
            raise ValueError("Unknown Set.Type")
        
    def isSubSetOf(self, x:Any) -> Result:
        if type(x) != Set: return Result.FALSE
        if x in self.allParents: return Result.TRUE

        if self.type == Set.Type.Finite:
            isSure = True
            for item in self.sureset:
                res = x.contains(item)
                if res == Result.FALSE: return Result.FALSE
                if res == Result.UNSURE: isSure = False
            for item in self.unsureset:
                res = x.contains(item)
                if res != Result.TRUE: isSure = False
            if isSure: return Result.TRUE
            return Result.UNSURE
        if x.type == Set.Type.Finite:
            if self.type == Set.Type.Descriptive: return Result.UNSURE
            if self.type == Set.Type.Union:
                for union in self.unions:
                    r = union.isSubSetOf(x)
                    if r != Result.TRUE: return Result.UNSURE
                return Result.TRUE
        return Result.UNSURE
    
    def __repr__(self):
        if self.type == Set.Type.Descriptive:
            return "Descriptive Set"
        if self.type == Set.Type.Finite:
            if self.unsureset == set():
                return f"{self.sureset}"
            else:
                return f"{self.sureset} and possibly {self.unsureset}"
        if self.type == Set.Type.Union:
            ustr = ', '.join([union.__repr__() for union in self.unions])
            return f"The union of {ustr}"
        raise ValueError("Unknown Set.Type")

class Relation:
    def __init__(self, setA:Set, setB:Set, relation:Callable[[Any, Any], Union[Result, bool]]):
        self.setA = setA
        self.setB = setB
        self.relation = relation

        if type(setA) != Set: raise TypeError("Expected setA to be of type Set")
        if type(setB) != Set: raise TypeError("Expected setB to be of type Set")
        if not isinstance(relation, Callable): raise TypeError("Expected relation parameter to be callable")
        if len(inspect.signature(relation).parameters) != 2: raise TypeError("Expected relation parameter to have exactly two arguments")

    def __call__(self, A, B)->Result:
        inA = self.setA.contains(A)
        inB = self.setB.contains(B)
        if inA & inB == Result.FALSE: return Result.FALSE

        try:
            result = self.relation(A, B)
        except:
            return Result.FALSE
        
        if inA & inB == Result.UNSURE: return Result.UNSURE

        if type(result) == bool:
            return Result.TRUE if result else Result.FALSE
        elif type(result) == Result:
            return result
        else:
            raise TypeError("This Relation has an incorrectly defined relation function, as it did not return a boolean or a Result")

class Function:
    def __init__(self, func:Callable[[Any], Any], domain:Set, codomain:Set=Set(Set.Descriptive), invFunc:Union[Callable[[Any], Any], None]=None):
        self.func = func
        self.domain = domain
        self.codomain = codomain
        self.invFunc = invFunc

        if type(domain) != Set: raise TypeError("Expected domain to be of type Set")
        if type(codomain) != Set: raise TypeError("Expected codomain to be of type Set")
        if not isinstance(func, Callable): raise TypeError("Expected func parameter to be callable")
        if len(inspect.signature(func).parameters) != 1: raise TypeError("Expected func parameter to have exactly one argument")

        self.relation = Relation(domain, codomain, lambda x, y: self(x)==y)
        if self.invFunc != None:
            if not isinstance(invFunc, Callable): raise TypeError("Expected invFunc parameter to be callable")
            if len(inspect.signature(invFunc).parameters) != 1: raise TypeError("Expected invFunc parameter to have exactly one argument")

    def hasInv(self)->bool:
        return self.invFunc != None

    def inv(self)->Self:
        if self.invFunc == None: raise ValueError("This function has no inverse specified")
        return Function(self.invFunc, self.codomain, self.domain, self.func)

    def __call__(self, x:Any)->Any:
        if self.domain.contains(x) == Result.FALSE: raise log("x is not part of the domain of this function", Severity.warn)

        try:
            y = self.func(x)
        except:
            raise log("x is not part of the domain of this function", Severity.warn)
        if self.codomain.contains(y) == Result.FALSE: log("This function is not correctly defined, as it yielded a result that was outside of its codomain", Severity.warn)
        
        if self.invFunc != None:
            try:
                original = self.invFunc(y)
            except:
                raise log("This functions inverse is incorrectly defined as composition with the original function did not yield the identity function", Severity.warn)
            if original != x: log("This functions inverse is incorrectly defined as composition with the original function did not yield the identity function", Severity.warn)
        
        return y

_ComplexNumbers = Set(Set.Descriptive, [lambda x:isinstance(x, Complex)])
_RealNumbers = Set(Set.Descriptive, [lambda x:isinstance(x, Real)], parents={_ComplexNumbers})
_AllFunctions = Set(Set.Descriptive, [lambda x:type(x)==Function])
_ComplexFunctions = Set(Set.Descriptive, [lambda x: _AllFunctions.contains(x) & x.domain.isSubset(_ComplexNumbers)], parents={_AllFunctions})

class SymbolicInfinity:
    def __init__(self, pos=True):
        if type(pos) != bool: raise TypeError("The argument 'pos' should be a boolean, signaling wether the infinity is positive or negative")
        self.pos = pos
    def __repr__(self)->str:
        return "\u221e" if self.pos else "-\u221e"
    def __eq__(self, y:Any)->bool:
        if type(y) != SymbolicInfinity: return NotImplemented
        return self.pos == y.pos
    def __neg__(self)->Self:
        return SymbolicInfinity(not self.pos)
    def __gt__(self, y:Any):
        if type(y) == SymbolicInfinity: return self.pos==True and y.pos==False
        if _RealNumbers.contains(y): return self.pos
        return NotImplemented
    def __lt__(self, y:Any):
        if type(y) == SymbolicInfinity: return self.pos==False and y.pos==True
        if _RealNumbers.contains(y): return not self.pos
        return NotImplemented
    def __ge__(self, y:Any):
        if self.__gt__(y) == NotImplemented: return NotImplemented
        return self == y or self > y
    def __le__(self, y:Any):
        if self.__lt__(y) == NotImplemented: return NotImplemented
        return self == y or self < y
    
SymInf = SymbolicInfinity(True)

class Interval:
    def __init__(self, a:Union[Real,SymbolicInfinity], b:Union[Real,SymbolicInfinity], includeA:bool=False, includeB:bool=False):
        self.set = Set(_RealNumbers, [lambda x: isinstance(x,Real) and x>=a and x<=b and (includeA or x!=a) and (includeB or x!=b)])
        self.a = a
        self.b = b
        self.includeA = includeA
        self.includeB = includeB
        if a == -SymInf and includeA: raise ValueError("-\u221e cannot be included. Set includeA to False")
        if b == SymInf and includeB: raise ValueError("\u221e cannot be included. Set includeB to False")
        if a == b and (not includeA or not includeB): raise ValueError("If a and b are equal, both includeA and includeB need to be True")
        if b < a: raise ValueError("a must be less than b")
    
    def contains(self, x:Any)->Result:
        return self.set.contains(x)

    def __repr__(self)->str:
        p1="[" if self.includeA else "]"
        p2="]" if self.includeB else "["
        return p1+str(self.a)+", "+str(self.b)+p2

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
            # TODO: Treat custom float types as special cases (f.e. a type that implements significant digits will have a custom operator to check if an instance is zero or not)
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
    C = _ComplexNumbers
    R = _RealNumbers
    Q = Set(Set.Descriptive, [lambda x:isinstance(x, Rational)], parents={_RealNumbers}) # any Real number that has numerator and denominator properties
    Z = Set(Set.Descriptive, [lambda x:isinstance(x, Integral)], parents={Q})
    N = Set(Set.Descriptive, [lambda x:Sets.Z.contains(x) and x >= 0], parents={Z}) # includes 0

    C2 = Set(Set.Descriptive, [lambda v:isinstance(v, Vec2)])
    R2 = Set(Set.Descriptive, [lambda v:Sets.C2.contains(v)] + [lambda v: Sets.R.contains(v[i]) for i in range(2)], parents={C2})
    Q2 = Set(Set.Descriptive, [lambda v:Sets.C2.contains(v)] + [lambda v: Sets.Q.contains(v[i]) for i in range(2)], parents={R2})

    C3 = Set(Set.Descriptive, [lambda v:isinstance(v, Vec3)])
    R3 = Set(Set.Descriptive, [lambda v:Sets.C3.contains(v)] + [lambda v: Sets.R.contains(v[i]) for i in range(3)], parents={C3})
    Q3 = Set(Set.Descriptive, [lambda v:Sets.C3.contains(v)] + [lambda v: Sets.Q.contains(v[i]) for i in range(3)], parents={R3})
    
    C4 = Set(Set.Descriptive, [lambda v:isinstance(v, Vec4)])
    R4 = Set(Set.Descriptive, [lambda v:Sets.C4.contains(v)] + [lambda v: Sets.R.contains(v[i]) for i in range(4)], parents={C4})
    Q4 = Set(Set.Descriptive, [lambda v:Sets.C4.contains(v)] + [lambda v: Sets.Q.contains(v[i]) for i in range(4)], parents={R4})