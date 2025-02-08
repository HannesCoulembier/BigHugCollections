from Tools.tester import Tester

import Tools.bhmath as math


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
        except TypeError:
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
        
        if math.Result.FALSE.__eq__("Dummy")  != NotImplemented: return False
        if math.Result.UNSURE.__eq__("Dummy") != NotImplemented: return False
        if math.Result.TRUE.__eq__("Dummy")   != NotImplemented: return False
        return True
    def OrOperator() -> bool:
        """Tests if the __or__ method works as intended"""
        if (math.Result.FALSE  | math.Result.FALSE)  != math.Result.FALSE:  return False
        if (math.Result.FALSE  | math.Result.UNSURE) != math.Result.UNSURE: return False
        if (math.Result.FALSE  | math.Result.TRUE)   != math.Result.TRUE:   return False
        if (math.Result.UNSURE | math.Result.FALSE)  != math.Result.UNSURE: return False
        if (math.Result.UNSURE | math.Result.UNSURE) != math.Result.UNSURE: return False
        if (math.Result.UNSURE | math.Result.TRUE)   != math.Result.TRUE:   return False
        if (math.Result.TRUE   | math.Result.FALSE)  != math.Result.TRUE:   return False
        if (math.Result.TRUE   | math.Result.UNSURE) != math.Result.TRUE:   return False
        if (math.Result.TRUE   | math.Result.TRUE)   != math.Result.TRUE:   return False

        if math.Result.FALSE.__or__("Dummy")  != NotImplemented: return False
        if math.Result.UNSURE.__or__("Dummy") != NotImplemented: return False
        if math.Result.TRUE.__or__("Dummy")   != NotImplemented: return False
        return True
    def AndOperator() -> bool:
        """Tests if the __and__ method works as intended"""
        if (math.Result.FALSE  & math.Result.FALSE)  != math.Result.FALSE:  return False
        if (math.Result.FALSE  & math.Result.UNSURE) != math.Result.FALSE:  return False
        if (math.Result.FALSE  & math.Result.TRUE)   != math.Result.FALSE:  return False
        if (math.Result.UNSURE & math.Result.FALSE)  != math.Result.FALSE:  return False
        if (math.Result.UNSURE & math.Result.UNSURE) != math.Result.UNSURE: return False
        if (math.Result.UNSURE & math.Result.TRUE)   != math.Result.UNSURE: return False
        if (math.Result.TRUE   & math.Result.FALSE)  != math.Result.FALSE:  return False
        if (math.Result.TRUE   & math.Result.UNSURE) != math.Result.UNSURE: return False
        if (math.Result.TRUE   & math.Result.TRUE)   != math.Result.TRUE:   return False

        if math.Result.FALSE.__and__("Dummy")  != NotImplemented: return False
        if math.Result.UNSURE.__and__("Dummy") != NotImplemented: return False
        if math.Result.TRUE.__and__("Dummy")   != NotImplemented: return False
        return True



TestMathResult()