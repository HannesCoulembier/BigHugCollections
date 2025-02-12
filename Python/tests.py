from Tools.bhtester import Tester

import Tools.bhmath as math

# ----- All individual tests ----------------------------------------------------------------------
class TestMathResult(Tester):
    """Tests if the Result class works as intended"""
    def ResultToBool() -> bool:
        """Tests if the __bool__ method works as intended"""
        try:
            T = bool(math.Result.TRUE)  # Converting Result.TRUE  should yield True
            F = bool(math.Result.FALSE) # Converting Result.FALSE should yield False
            if T != True or F != False: return False
        except:
            return False
        
        try:
            U = bool(math.Result.UNSURE) # Trying to convert Result.UNSURE should raise a TypeError
        except NotImplementedError:
            return True
        return False # This will run when either the conversion in the try block succeeded or the wrong exception was thrown
    def StringRepresentation() -> bool:
        """Tests if the __str__ and __repr__ methods work as intended"""

        Tr = math.Result.TRUE.__repr__()
        Ur = math.Result.UNSURE.__repr__()
        Fr = math.Result.FALSE.__repr__()

        Ts = math.Result.TRUE.__str__()
        Us = math.Result.UNSURE.__str__()
        Fs = math.Result.FALSE.__str__()

        return [Tr, Ur, Fr, Ts, Us, Fs] == ["TRUE", "UNSURE", "FALSE", "TRUE", "UNSURE", "FALSE"]
    def EqualityOperator() -> bool:
        """Tests if the __eq__ method works as intended"""
        if (math.Result.FALSE  == math.Result.FALSE)  != True:  return False
        if (math.Result.FALSE  == math.Result.UNSURE) != False: return False
        if (math.Result.FALSE  == math.Result.TRUE)   != False: return False
        if (math.Result.UNSURE == math.Result.FALSE)  != False: return False
        if (math.Result.UNSURE == math.Result.UNSURE) != True:  return False
        if (math.Result.UNSURE == math.Result.TRUE)   != False: return False
        if (math.Result.TRUE   == math.Result.FALSE)  != False: return False
        if (math.Result.TRUE   == math.Result.UNSURE) != False: return False
        if (math.Result.TRUE   == math.Result.TRUE)   != True:  return False

        if (math.Result.FALSE == False)  != True:  return False
        if (math.Result.FALSE == True)   != False: return False
        if (math.Result.UNSURE == False) != False: return False
        if (math.Result.UNSURE == True)  != False: return False
        if (math.Result.TRUE  == False)  != False: return False
        if (math.Result.TRUE  == True)   != True:  return False
        if (False == math.Result.FALSE)  != True:  return False
        if (True  == math.Result.FALSE)  != False: return False
        if (False == math.Result.UNSURE) != False: return False
        if (True  == math.Result.UNSURE) != False: return False
        if (False == math.Result.TRUE)   != False: return False
        if (True  == math.Result.TRUE)   != True:  return False
        
        if math.Result.FALSE.__eq__("Dummy")  != NotImplemented: return False
        if math.Result.UNSURE.__eq__("Dummy") != NotImplemented: return False
        if math.Result.TRUE.__eq__("Dummy")   != NotImplemented: return False
        return True
    def OrOperator() -> bool:
        """Tests if the __ror__ methods work as intended"""
        if (math.Result.FALSE  | math.Result.FALSE)  != math.Result.FALSE:  return False
        if (math.Result.FALSE  | math.Result.UNSURE) != math.Result.UNSURE: return False
        if (math.Result.FALSE  | math.Result.TRUE)   != math.Result.TRUE:   return False
        if (math.Result.UNSURE | math.Result.FALSE)  != math.Result.UNSURE: return False
        if (math.Result.UNSURE | math.Result.UNSURE) != math.Result.UNSURE: return False
        if (math.Result.UNSURE | math.Result.TRUE)   != math.Result.TRUE:   return False
        if (math.Result.TRUE   | math.Result.FALSE)  != math.Result.TRUE:   return False
        if (math.Result.TRUE   | math.Result.UNSURE) != math.Result.TRUE:   return False
        if (math.Result.TRUE   | math.Result.TRUE)   != math.Result.TRUE:   return False
        if (math.Result.FALSE  | False) != math.Result.FALSE:  return False
        if (math.Result.FALSE  | True)  != math.Result.TRUE:   return False
        if (math.Result.UNSURE | False) != math.Result.UNSURE: return False
        if (math.Result.UNSURE | True)  != math.Result.TRUE:   return False
        if (math.Result.TRUE   | False) != math.Result.TRUE:   return False
        if (math.Result.TRUE   | True)  != math.Result.TRUE:   return False
        if (False | math.Result.FALSE)  != math.Result.FALSE:  return False
        if (True  | math.Result.FALSE)  != math.Result.TRUE:   return False
        if (False | math.Result.UNSURE) != math.Result.UNSURE: return False
        if (True  | math.Result.UNSURE) != math.Result.TRUE:   return False
        if (False | math.Result.TRUE)   != math.Result.TRUE:   return False
        if (True  | math.Result.TRUE)   != math.Result.TRUE:   return False

        if math.Result.FALSE.__or__("Dummy")  != NotImplemented: return False
        if math.Result.UNSURE.__or__("Dummy") != NotImplemented: return False
        if math.Result.TRUE.__or__("Dummy")   != NotImplemented: return False
        if math.Result.FALSE.__ror__("Dummy")  != NotImplemented: return False
        if math.Result.UNSURE.__ror__("Dummy") != NotImplemented: return False
        if math.Result.TRUE.__ror__("Dummy")   != NotImplemented: return False
        return True
    def AndOperator() -> bool:
        """Tests if the __and__ and __rand__ methods work as intended"""
        if (math.Result.FALSE  & math.Result.FALSE)  != math.Result.FALSE:  return False
        if (math.Result.FALSE  & math.Result.UNSURE) != math.Result.FALSE:  return False
        if (math.Result.FALSE  & math.Result.TRUE)   != math.Result.FALSE:  return False
        if (math.Result.UNSURE & math.Result.FALSE)  != math.Result.FALSE:  return False
        if (math.Result.UNSURE & math.Result.UNSURE) != math.Result.UNSURE: return False
        if (math.Result.UNSURE & math.Result.TRUE)   != math.Result.UNSURE: return False
        if (math.Result.TRUE   & math.Result.FALSE)  != math.Result.FALSE:  return False
        if (math.Result.TRUE   & math.Result.UNSURE) != math.Result.UNSURE: return False
        if (math.Result.TRUE   & math.Result.TRUE)   != math.Result.TRUE:   return False
        if (math.Result.FALSE  & False) != math.Result.FALSE:  return False
        if (math.Result.FALSE  & True)  != math.Result.FALSE:  return False
        if (math.Result.UNSURE & False) != math.Result.FALSE:  return False
        if (math.Result.UNSURE & True)  != math.Result.UNSURE: return False
        if (math.Result.TRUE   & False) != math.Result.FALSE:  return False
        if (math.Result.TRUE   & True)  != math.Result.TRUE:   return False
        if (False & math.Result.FALSE)  != math.Result.FALSE:  return False
        if (True  & math.Result.FALSE)  != math.Result.FALSE:  return False
        if (False & math.Result.UNSURE) != math.Result.FALSE:  return False
        if (True  & math.Result.UNSURE) != math.Result.UNSURE: return False
        if (False & math.Result.TRUE)   != math.Result.FALSE:  return False
        if (True  & math.Result.TRUE)   != math.Result.TRUE:   return False

        if math.Result.FALSE.__and__("Dummy")  != NotImplemented: return False
        if math.Result.UNSURE.__and__("Dummy") != NotImplemented: return False
        if math.Result.TRUE.__and__("Dummy")   != NotImplemented: return False
        if math.Result.FALSE.__rand__("Dummy")  != NotImplemented: return False
        if math.Result.UNSURE.__rand__("Dummy") != NotImplemented: return False
        if math.Result.TRUE.__rand__("Dummy")   != NotImplemented: return False
        return True

class TestMathSymbolicInfinity(Tester):
    """Tests if the SymbolicInfinity class works as intended"""
    def Constructor() -> bool:
        """Tests if the __init__ method works as intended"""
        try:
            T1 = math.SymbolicInfinity(True)
            T2 = math.SymbolicInfinity()
            F = math.SymbolicInfinity(False)
            if T1 != T2: return False
        except:
            return False
        try:
            math.SymbolicInfinity("Dummy")
            return False
        except:
            return True
    def StringRepresentation() -> bool:
        """Tests if the __str__ and __repr__ methods work as intended"""

        Pr = math.SymbolicInfinity(True).__repr__()
        Nr = math.SymbolicInfinity(False).__repr__()

        Ps = math.SymbolicInfinity(True).__str__()
        Ns = math.SymbolicInfinity(False).__str__()

        return [Pr, Nr, Ps, Ns] == ["\u221e", "-\u221e", "\u221e", "-\u221e"]
    def EqualityOperator() -> bool:
        """Tests if the __eq__ method works as intended"""
        if (math.SymbolicInfinity(False) == math.SymbolicInfinity(False)) != True:  return False
        if (math.SymbolicInfinity(False) == math.SymbolicInfinity(True))  != False: return False
        if (math.SymbolicInfinity(True)  == math.SymbolicInfinity(False)) != False: return False
        if (math.SymbolicInfinity(True)  == math.SymbolicInfinity(True))  != True:  return False
        
        if math.SymbolicInfinity(False).__eq__("Dummy")  != NotImplemented: return False
        if math.SymbolicInfinity(True).__eq__("Dummy")   != NotImplemented: return False
        return True
    def NegationOperator() -> bool:
        """Tests if the __neg__ method works as intended"""
        if -math.SymbolicInfinity(False) != math.SymbolicInfinity(True):  return False
        if -math.SymbolicInfinity(True)  != math.SymbolicInfinity(False): return False
        return True
    def ComparisonOperators() -> bool:
        """Tests if the __gt__, __lt__, __ge__ and __le__ methods work as intended"""
        P = math.SymbolicInfinity()
        N = -P
        if (N >  N) != False: return False
        if (N >  P) != False: return False
        if (P >  N) != True:  return False
        if (P >  P) != False: return False
        if (N >= N) != True:  return False
        if (N >= P) != False: return False
        if (P >= N) != True:  return False
        if (P >= P) != True:  return False
        if (N <  N) != False: return False
        if (N <  P) != True:  return False
        if (P <  N) != False: return False
        if (P <  P) != False: return False
        if (N <= N) != True:  return False
        if (N <= P) != True:  return False
        if (P <= N) != False: return False
        if (P <= P) != True:  return False

        if (N >  0) != False: return False
        if (P >  0) != True:  return False
        if (N >= 0) != False: return False
        if (P >= 0) != True:  return False
        if (N <  0) != True:  return False
        if (P <  0) != False: return False
        if (N <= 0) != True:  return False
        if (P <= 0) != False: return False
        if (N >  28.71) != False: return False
        if (P >  28.71) != True:  return False
        if (N >= 28.71) != False: return False
        if (P >= 28.71) != True:  return False
        if (N <  28.71) != True:  return False
        if (P <  28.71) != False: return False
        if (N <= 28.71) != True:  return False
        if (P <= 28.71) != False: return False

        if N.__gt__("Dummy") != NotImplemented: return False
        if P.__gt__("Dummy") != NotImplemented: return False
        if N.__ge__("Dummy") != NotImplemented: return False
        if P.__ge__("Dummy") != NotImplemented: return False
        if N.__lt__("Dummy") != NotImplemented: return False
        if P.__lt__("Dummy") != NotImplemented: return False
        if N.__le__("Dummy") != NotImplemented: return False
        if P.__le__("Dummy") != NotImplemented: return False
        
        if N.__gt__(1.2+0.3j) != NotImplemented: return False
        if P.__gt__(1.2+0.3j) != NotImplemented: return False
        if N.__ge__(1.2+0.3j) != NotImplemented: return False
        if P.__ge__(1.2+0.3j) != NotImplemented: return False
        if N.__lt__(1.2+0.3j) != NotImplemented: return False
        if P.__lt__(1.2+0.3j) != NotImplemented: return False
        if N.__le__(1.2+0.3j) != NotImplemented: return False
        if P.__le__(1.2+0.3j) != NotImplemented: return False
        return True


# ----- Grouped tests -----------------------------------------------------------------------------
class TestAllMathTests(Tester):
    """Runs all the Math tests"""
    def Result() -> bool: return TestMathResult(True)
    def SymbolicInfinity() -> bool: return TestMathSymbolicInfinity(True)

TestAllMathTests()