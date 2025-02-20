from Tools.bhtester import Tester, ReprTester, TestResult, PASSED, FAILED, TestBHTester

import Tools.bhmath as math

# ----- All individual tests ----------------------------------------------------------------------
class TestMathResult(Tester):
    """Tests if the Result class works as intended"""
    def ResultToBool() -> TestResult:
        """Tests if the __bool__ method works as intended"""
        try:
            T = bool(math.Result.TRUE)  # Converting Result.TRUE  should yield True
            F = bool(math.Result.FALSE) # Converting Result.FALSE should yield False
            if T != True or F != False: return FAILED
        except:
            return FAILED
        
        try:
            U = bool(math.Result.UNSURE) # Trying to convert Result.UNSURE should raise a TypeError
        except NotImplementedError:
            return PASSED
        return FAILED # This will run when either the conversion in the try block succeeded or the wrong exception was thrown
    StringRepresentation = ReprTester([
        (math.Result.TRUE,      "TRUE"),
        (math.Result.UNSURE,    "UNSURE"),
        (math.Result.FALSE,     "FALSE"),
    ])
    def EqualityOperator() -> TestResult:
        """Tests if the __eq__ method works as intended"""
        if (math.Result.FALSE  == math.Result.FALSE)  != True:  return FAILED
        if (math.Result.FALSE  == math.Result.UNSURE) != False: return FAILED
        if (math.Result.FALSE  == math.Result.TRUE)   != False: return FAILED
        if (math.Result.UNSURE == math.Result.FALSE)  != False: return FAILED
        if (math.Result.UNSURE == math.Result.UNSURE) != True:  return FAILED
        if (math.Result.UNSURE == math.Result.TRUE)   != False: return FAILED
        if (math.Result.TRUE   == math.Result.FALSE)  != False: return FAILED
        if (math.Result.TRUE   == math.Result.UNSURE) != False: return FAILED
        if (math.Result.TRUE   == math.Result.TRUE)   != True:  return FAILED

        if (math.Result.FALSE == False)  != True:  return FAILED
        if (math.Result.FALSE == True)   != False: return FAILED
        if (math.Result.UNSURE == False) != False: return FAILED
        if (math.Result.UNSURE == True)  != False: return FAILED
        if (math.Result.TRUE  == False)  != False: return FAILED
        if (math.Result.TRUE  == True)   != True:  return FAILED
        if (False == math.Result.FALSE)  != True:  return FAILED
        if (True  == math.Result.FALSE)  != False: return FAILED
        if (False == math.Result.UNSURE) != False: return FAILED
        if (True  == math.Result.UNSURE) != False: return FAILED
        if (False == math.Result.TRUE)   != False: return FAILED
        if (True  == math.Result.TRUE)   != True:  return FAILED
        
        if math.Result.FALSE.__eq__("Dummy")  != NotImplemented: return FAILED
        if math.Result.UNSURE.__eq__("Dummy") != NotImplemented: return FAILED
        if math.Result.TRUE.__eq__("Dummy")   != NotImplemented: return FAILED
        return PASSED
    def OrOperator() -> TestResult:
        """Tests if the __ror__ methods work as intended"""
        if (math.Result.FALSE  | math.Result.FALSE)  != math.Result.FALSE:  return FAILED
        if (math.Result.FALSE  | math.Result.UNSURE) != math.Result.UNSURE: return FAILED
        if (math.Result.FALSE  | math.Result.TRUE)   != math.Result.TRUE:   return FAILED
        if (math.Result.UNSURE | math.Result.FALSE)  != math.Result.UNSURE: return FAILED
        if (math.Result.UNSURE | math.Result.UNSURE) != math.Result.UNSURE: return FAILED
        if (math.Result.UNSURE | math.Result.TRUE)   != math.Result.TRUE:   return FAILED
        if (math.Result.TRUE   | math.Result.FALSE)  != math.Result.TRUE:   return FAILED
        if (math.Result.TRUE   | math.Result.UNSURE) != math.Result.TRUE:   return FAILED
        if (math.Result.TRUE   | math.Result.TRUE)   != math.Result.TRUE:   return FAILED
        if (math.Result.FALSE  | False) != math.Result.FALSE:  return FAILED
        if (math.Result.FALSE  | True)  != math.Result.TRUE:   return FAILED
        if (math.Result.UNSURE | False) != math.Result.UNSURE: return FAILED
        if (math.Result.UNSURE | True)  != math.Result.TRUE:   return FAILED
        if (math.Result.TRUE   | False) != math.Result.TRUE:   return FAILED
        if (math.Result.TRUE   | True)  != math.Result.TRUE:   return FAILED
        if (False | math.Result.FALSE)  != math.Result.FALSE:  return FAILED
        if (True  | math.Result.FALSE)  != math.Result.TRUE:   return FAILED
        if (False | math.Result.UNSURE) != math.Result.UNSURE: return FAILED
        if (True  | math.Result.UNSURE) != math.Result.TRUE:   return FAILED
        if (False | math.Result.TRUE)   != math.Result.TRUE:   return FAILED
        if (True  | math.Result.TRUE)   != math.Result.TRUE:   return FAILED

        if math.Result.FALSE.__or__("Dummy")  != NotImplemented: return FAILED
        if math.Result.UNSURE.__or__("Dummy") != NotImplemented: return FAILED
        if math.Result.TRUE.__or__("Dummy")   != NotImplemented: return FAILED
        if math.Result.FALSE.__ror__("Dummy")  != NotImplemented: return FAILED
        if math.Result.UNSURE.__ror__("Dummy") != NotImplemented: return FAILED
        if math.Result.TRUE.__ror__("Dummy")   != NotImplemented: return FAILED
        return PASSED
    def AndOperator() -> TestResult:
        """Tests if the __and__ and __rand__ methods work as intended"""
        if (math.Result.FALSE  & math.Result.FALSE)  != math.Result.FALSE:  return FAILED
        if (math.Result.FALSE  & math.Result.UNSURE) != math.Result.FALSE:  return FAILED
        if (math.Result.FALSE  & math.Result.TRUE)   != math.Result.FALSE:  return FAILED
        if (math.Result.UNSURE & math.Result.FALSE)  != math.Result.FALSE:  return FAILED
        if (math.Result.UNSURE & math.Result.UNSURE) != math.Result.UNSURE: return FAILED
        if (math.Result.UNSURE & math.Result.TRUE)   != math.Result.UNSURE: return FAILED
        if (math.Result.TRUE   & math.Result.FALSE)  != math.Result.FALSE:  return FAILED
        if (math.Result.TRUE   & math.Result.UNSURE) != math.Result.UNSURE: return FAILED
        if (math.Result.TRUE   & math.Result.TRUE)   != math.Result.TRUE:   return FAILED
        if (math.Result.FALSE  & False) != math.Result.FALSE:  return FAILED
        if (math.Result.FALSE  & True)  != math.Result.FALSE:  return FAILED
        if (math.Result.UNSURE & False) != math.Result.FALSE:  return FAILED
        if (math.Result.UNSURE & True)  != math.Result.UNSURE: return FAILED
        if (math.Result.TRUE   & False) != math.Result.FALSE:  return FAILED
        if (math.Result.TRUE   & True)  != math.Result.TRUE:   return FAILED
        if (False & math.Result.FALSE)  != math.Result.FALSE:  return FAILED
        if (True  & math.Result.FALSE)  != math.Result.FALSE:  return FAILED
        if (False & math.Result.UNSURE) != math.Result.FALSE:  return FAILED
        if (True  & math.Result.UNSURE) != math.Result.UNSURE: return FAILED
        if (False & math.Result.TRUE)   != math.Result.FALSE:  return FAILED
        if (True  & math.Result.TRUE)   != math.Result.TRUE:   return FAILED

        if math.Result.FALSE.__and__("Dummy")  != NotImplemented: return FAILED
        if math.Result.UNSURE.__and__("Dummy") != NotImplemented: return FAILED
        if math.Result.TRUE.__and__("Dummy")   != NotImplemented: return FAILED
        if math.Result.FALSE.__rand__("Dummy")  != NotImplemented: return FAILED
        if math.Result.UNSURE.__rand__("Dummy") != NotImplemented: return FAILED
        if math.Result.TRUE.__rand__("Dummy")   != NotImplemented: return FAILED
        return PASSED

class TestMathSymbolicInfinity(Tester):
    """Tests if the SymbolicInfinity class works as intended"""
    def Constructor() -> TestResult:
        """Tests if the __init__ method works as intended"""
        try:
            T1 = math.SymbolicInfinity(True)
            T2 = math.SymbolicInfinity()
            F = math.SymbolicInfinity(False)
            if T1 != T2: return FAILED
        except:
            return FAILED
        try:
            math.SymbolicInfinity("Dummy")
            return FAILED
        except:
            return PASSED
    StringRepresentation = ReprTester([
        (math.SymbolicInfinity(True), "\u221e"),
        (math.SymbolicInfinity(False), "-\u221e"),
    ])
    def EqualityOperator() -> TestResult:
        """Tests if the __eq__ method works as intended"""
        if (math.SymbolicInfinity(False) == math.SymbolicInfinity(False)) != True:  return FAILED
        if (math.SymbolicInfinity(False) == math.SymbolicInfinity(True))  != False: return FAILED
        if (math.SymbolicInfinity(True)  == math.SymbolicInfinity(False)) != False: return FAILED
        if (math.SymbolicInfinity(True)  == math.SymbolicInfinity(True))  != True:  return FAILED
        
        if math.SymbolicInfinity(False).__eq__("Dummy")  != NotImplemented: return FAILED
        if math.SymbolicInfinity(True).__eq__("Dummy")   != NotImplemented: return FAILED
        return PASSED
    def NegationOperator() -> TestResult:
        """Tests if the __neg__ method works as intended"""
        if -math.SymbolicInfinity(False) != math.SymbolicInfinity(True):  return FAILED
        if -math.SymbolicInfinity(True)  != math.SymbolicInfinity(False): return FAILED
        return PASSED
    def ComparisonOperators() -> TestResult:
        """Tests if the __gt__, __lt__, __ge__ and __le__ methods work as intended"""
        P = math.SymbolicInfinity()
        N = -P
        if (N >  N) != False: return FAILED
        if (N >  P) != False: return FAILED
        if (P >  N) != True:  return FAILED
        if (P >  P) != False: return FAILED
        if (N >= N) != True:  return FAILED
        if (N >= P) != False: return FAILED
        if (P >= N) != True:  return FAILED
        if (P >= P) != True:  return FAILED
        if (N <  N) != False: return FAILED
        if (N <  P) != True:  return FAILED
        if (P <  N) != False: return FAILED
        if (P <  P) != False: return FAILED
        if (N <= N) != True:  return FAILED
        if (N <= P) != True:  return FAILED
        if (P <= N) != False: return FAILED
        if (P <= P) != True:  return FAILED

        if (N >  0) != False: return FAILED
        if (P >  0) != True:  return FAILED
        if (N >= 0) != False: return FAILED
        if (P >= 0) != True:  return FAILED
        if (N <  0) != True:  return FAILED
        if (P <  0) != False: return FAILED
        if (N <= 0) != True:  return FAILED
        if (P <= 0) != False: return FAILED
        if (N >  28.71) != False: return FAILED
        if (P >  28.71) != True:  return FAILED
        if (N >= 28.71) != False: return FAILED
        if (P >= 28.71) != True:  return FAILED
        if (N <  28.71) != True:  return FAILED
        if (P <  28.71) != False: return FAILED
        if (N <= 28.71) != True:  return FAILED
        if (P <= 28.71) != False: return FAILED

        if N.__gt__("Dummy") != NotImplemented: return FAILED
        if P.__gt__("Dummy") != NotImplemented: return FAILED
        if N.__ge__("Dummy") != NotImplemented: return FAILED
        if P.__ge__("Dummy") != NotImplemented: return FAILED
        if N.__lt__("Dummy") != NotImplemented: return FAILED
        if P.__lt__("Dummy") != NotImplemented: return FAILED
        if N.__le__("Dummy") != NotImplemented: return FAILED
        if P.__le__("Dummy") != NotImplemented: return FAILED
        
        if N.__gt__(1.2+0.3j) != NotImplemented: return FAILED
        if P.__gt__(1.2+0.3j) != NotImplemented: return FAILED
        if N.__ge__(1.2+0.3j) != NotImplemented: return FAILED
        if P.__ge__(1.2+0.3j) != NotImplemented: return FAILED
        if N.__lt__(1.2+0.3j) != NotImplemented: return FAILED
        if P.__lt__(1.2+0.3j) != NotImplemented: return FAILED
        if N.__le__(1.2+0.3j) != NotImplemented: return FAILED
        if P.__le__(1.2+0.3j) != NotImplemented: return FAILED
        return PASSED

class TestMathSet(Tester):
    """Tests if the Set class works as intended"""
    class Constructor(Tester):
        """Tests if the __init__ method works as intended"""
        def InputChecks() -> TestResult:
            """Tests if all input is correctly (type) checked"""
            try:
                math.Set("dummy")
                return FAILED
            except(TypeError): pass
            try:
                math.Set(set(), "dummy")
                return FAILED
            except(TypeError): pass
            try:
                math.Set(set(), ["dummy"])
                return FAILED
            except(TypeError): pass
            math.Set(set(), [])
            try:
                math.Set(set(), [], "dummy")
                return FAILED
            except(TypeError): pass
            try:
                math.Set(set(), [], set("dummy"))
                return FAILED
            except(TypeError): pass

            badBoy = math.Set(math.Set.Descriptive)
            badBoy.type = 7
            try:
                math.Set(badBoy)
                return FAILED
            except(ValueError): pass
            return PASSED
        def Empty() -> TestResult:
            """Tests the empty set constructor"""
            empty1 = math.Set(math.Set.Empty)
            empty2 = math.Set(math.Set.Empty, [lambda x: False])

            if empty1.conditions                                        != []:                      return FAILED
            if empty1.sureset                                           != set():                   return FAILED
            if empty1.unsureset                                         != set():                   return FAILED
            if empty1.parents                                           != set():                   return FAILED
            if empty1.allParents                                        != set():                   return FAILED
            if empty1.type                                              != math.Set.Type.Finite:    return FAILED
            if empty1.unions                                            != []:                      return FAILED
            
            if empty2.conditions                                        != []:                      return FAILED
            if empty2.sureset                                           != set():                   return FAILED
            if empty2.unsureset                                         != set():                   return FAILED
            if empty2.parents                                           != set():                   return FAILED
            if empty2.allParents                                        != set():                   return FAILED
            if empty2.type                                              != math.Set.Type.Finite:    return FAILED
            if empty2.unions                                            != []:                      return FAILED

            return PASSED
        def Descriptive() -> TestResult:
            """Tests Descriptive Sets constructors"""
            desc1 = math.Set(math.Set.Descriptive)
            desc2 = math.Set(math.Set.Descriptive, [lambda x: math.Result.UNSURE])
            desc3 = math.Set(math.Set.Descriptive, parents={math.Sets.R})
            desc4 = math.Set(math.Set.Descriptive, [lambda x: math.Result.UNSURE], parents={math.Sets.R})

            if len(desc1.conditions)    != 1:                           return FAILED
            if     desc1.sureset        != set():                       return FAILED
            if     desc1.unsureset      != set():                       return FAILED
            if     desc1.parents        != set():                       return FAILED
            if     desc1.allParents     != set():                       return FAILED
            if     desc1.type           != math.Set.Type.Descriptive:   return FAILED
            if     desc1.unions         != []:                          return FAILED
            
            if len(desc2.conditions)    != 1:                           return FAILED
            if     desc2.sureset        != set():                       return FAILED
            if     desc2.unsureset      != set():                       return FAILED
            if     desc2.parents        != set():                       return FAILED
            if     desc2.allParents     != set():                       return FAILED
            if     desc2.type           != math.Set.Type.Descriptive:   return FAILED
            if     desc2.unions         != []:                          return FAILED
            
            if len(desc3.conditions)    != 2:                           return FAILED
            if     desc3.sureset        != set():                       return FAILED
            if     desc3.unsureset      != set():                       return FAILED
            if     desc3.parents        != {math.Sets.R}:               return FAILED
            if     desc3.allParents     != {math.Sets.R, math.Sets.C}:  return FAILED
            if     desc3.type           != math.Set.Type.Descriptive:   return FAILED
            if     desc3.unions         != []:                          return FAILED
            
            if len(desc4.conditions)    != 2:                           return FAILED
            if     desc4.sureset        != set():                       return FAILED
            if     desc4.unsureset      != set():                       return FAILED
            if     desc4.parents        != {math.Sets.R}:               return FAILED
            if     desc4.allParents     != {math.Sets.R, math.Sets.C}:  return FAILED
            if     desc4.type           != math.Set.Type.Descriptive:   return FAILED
            if     desc4.unions         != []:                          return FAILED

            return PASSED
        def ListOrSet() -> TestResult:
            """Tests constructions that use a list or a set as a base"""
            lst1 = math.Set([])
            lst2 = math.Set([1, 2, "3"])
            lst3 = math.Set([1, 2, "3"], [lambda x: math.Result.FALSE])
            lst4 = math.Set([1, 2, "3"], [lambda x: math.Result.UNSURE])
            lst5 = math.Set([1, 2, "3"], [lambda x: math.Result.TRUE])
            lst6 = math.Set([1, 2, "3"], parents={math.Sets.R})
            lst7 = math.Set([1, 2, "3"], [lambda x: math.Result.UNSURE], parents={math.Sets.R})

            set1 = math.Set([])
            set2 = math.Set([1, 2, "3"])
            set3 = math.Set([1, 2, "3"], [lambda x: math.Result.FALSE])
            set4 = math.Set([1, 2, "3"], [lambda x: math.Result.UNSURE])
            set5 = math.Set([1, 2, "3"], [lambda x: math.Result.TRUE])
            set6 = math.Set([1, 2, "3"], parents={math.Sets.R})
            set7 = math.Set([1, 2, "3"], [lambda x: math.Result.UNSURE], parents={math.Sets.R})

            if len(lst1.conditions)    != 0:                            return FAILED
            if     lst1.sureset        != set():                        return FAILED
            if     lst1.unsureset      != set():                        return FAILED
            if     lst1.parents        != set():                        return FAILED
            if     lst1.allParents     != set():                        return FAILED
            if     lst1.type           != math.Set.Type.Finite:         return FAILED
            if     lst1.unions         != []:                           return FAILED
            if len(lst2.conditions)    != 0:                            return FAILED
            if     lst2.sureset        != {1, 2, "3"}:                  return FAILED
            if     lst2.unsureset      != set():                        return FAILED
            if     lst2.parents        != set():                        return FAILED
            if     lst2.allParents     != set():                        return FAILED
            if     lst2.type           != math.Set.Type.Finite:         return FAILED
            if     lst2.unions         != []:                           return FAILED
            if len(lst3.conditions)    != 0:                            return FAILED
            if     lst3.sureset        != set():                        return FAILED
            if     lst3.unsureset      != set():                        return FAILED
            if     lst3.parents        != set():                        return FAILED
            if     lst3.allParents     != set():                        return FAILED
            if     lst3.type           != math.Set.Type.Finite:         return FAILED
            if     lst3.unions         != []:                           return FAILED
            if len(lst4.conditions)    != 0:                            return FAILED
            if     lst4.sureset        != set():                        return FAILED
            if     lst4.unsureset      != {1, 2, "3"}:                  return FAILED
            if     lst4.parents        != set():                        return FAILED
            if     lst4.allParents     != set():                        return FAILED
            if     lst4.type           != math.Set.Type.Finite:         return FAILED
            if     lst4.unions         != []:                           return FAILED
            if len(lst5.conditions)    != 0:                            return FAILED
            if     lst5.sureset        != {1, 2, "3"}:                  return FAILED
            if     lst5.unsureset      != set():                        return FAILED
            if     lst5.parents        != set():                        return FAILED
            if     lst5.allParents     != set():                        return FAILED
            if     lst5.type           != math.Set.Type.Finite:         return FAILED
            if     lst5.unions         != []:                           return FAILED
            if len(lst6.conditions)    != 0:                            return FAILED
            if     lst6.sureset        != {1, 2}:                       return FAILED
            if     lst6.unsureset      != set():                        return FAILED
            if     lst6.parents        != {math.Sets.R}:                return FAILED
            if     lst6.allParents     != {math.Sets.R, math.Sets.C}:   return FAILED
            if     lst6.type           != math.Set.Type.Finite:         return FAILED
            if     lst6.unions         != []:                           return FAILED
            if len(lst7.conditions)    != 0:                            return FAILED
            if     lst7.sureset        != set():                        return FAILED
            if     lst7.unsureset      != {1, 2}:                       return FAILED
            if     lst7.parents        != {math.Sets.R}:                return FAILED
            if     lst7.allParents     != {math.Sets.R, math.Sets.C}:   return FAILED
            if     lst7.type           != math.Set.Type.Finite:         return FAILED
            if     lst7.unions         != []:                           return FAILED

            if len(set1.conditions)    != 0:                            return FAILED
            if     set1.sureset        != set():                        return FAILED
            if     set1.unsureset      != set():                        return FAILED
            if     set1.parents        != set():                        return FAILED
            if     set1.allParents     != set():                        return FAILED
            if     set1.type           != math.Set.Type.Finite:         return FAILED
            if     set1.unions         != []:                           return FAILED
            if len(set2.conditions)    != 0:                            return FAILED
            if     set2.sureset        != {1, 2, "3"}:                  return FAILED
            if     set2.unsureset      != set():                        return FAILED
            if     set2.parents        != set():                        return FAILED
            if     set2.allParents     != set():                        return FAILED
            if     set2.type           != math.Set.Type.Finite:         return FAILED
            if     set2.unions         != []:                           return FAILED
            if len(set3.conditions)    != 0:                            return FAILED
            if     set3.sureset        != set():                        return FAILED
            if     set3.unsureset      != set():                        return FAILED
            if     set3.parents        != set():                        return FAILED
            if     set3.allParents     != set():                        return FAILED
            if     set3.type           != math.Set.Type.Finite:         return FAILED
            if     set3.unions         != []:                           return FAILED
            if len(set4.conditions)    != 0:                            return FAILED
            if     set4.sureset        != set():                        return FAILED
            if     set4.unsureset      != {1, 2, "3"}:                  return FAILED
            if     set4.parents        != set():                        return FAILED
            if     set4.allParents     != set():                        return FAILED
            if     set4.type           != math.Set.Type.Finite:         return FAILED
            if     set4.unions         != []:                           return FAILED
            if len(set5.conditions)    != 0:                            return FAILED
            if     set5.sureset        != {1, 2, "3"}:                  return FAILED
            if     set5.unsureset      != set():                        return FAILED
            if     set5.parents        != set():                        return FAILED
            if     set5.allParents     != set():                        return FAILED
            if     set5.type           != math.Set.Type.Finite:         return FAILED
            if     set5.unions         != []:                           return FAILED
            if len(set6.conditions)    != 0:                            return FAILED
            if     set6.sureset        != {1, 2}:                       return FAILED
            if     set6.unsureset      != set():                        return FAILED
            if     set6.parents        != {math.Sets.R}:                return FAILED
            if     set6.allParents     != {math.Sets.R, math.Sets.C}:   return FAILED
            if     set6.type           != math.Set.Type.Finite:         return FAILED
            if     set6.unions         != []:                           return FAILED
            if len(set7.conditions)    != 0:                            return FAILED
            if     set7.sureset        != set():                        return FAILED
            if     set7.unsureset      != {1, 2}:                       return FAILED
            if     set7.parents        != {math.Sets.R}:                return FAILED
            if     set7.allParents     != {math.Sets.R, math.Sets.C}:   return FAILED
            if     set7.type           != math.Set.Type.Finite:         return FAILED
            if     set7.unions         != []:                           return FAILED

            return PASSED
        def Set() -> TestResult:
            """Tests if Sets are correctly constructed from other Sets"""
            finSure = math.Set({1, 2, 3})
            finUnsure = math.Set({1, 2, 3}, [lambda x: math.Result.UNSURE])
            empty = math.Set(math.Set.Empty)
            desc = math.Set(math.Set.Descriptive)
            descCond = math.Set(math.Set.Descriptive, [lambda x: math.Result.UNSURE])
            union = descCond | finSure

            s1 = math.Set(finSure)
            s2 = math.Set(finUnsure)
            s3 = math.Set(empty)
            s4 = math.Set(finUnsure, [lambda x: math.Result.FALSE])
            s5 = math.Set(desc)
            s6 = math.Set(desc, [lambda x: math.Result.UNSURE])
            s7 = math.Set(descCond)
            s8 = math.Set(descCond, [lambda x: math.Result.UNSURE])
            s9 = math.Set(descCond, [lambda x: math.Result.UNSURE, lambda x: math.Result.FALSE])
            s10 = math.Set(union)
            s11 = math.Set(union, [lambda x: math.Result.UNSURE], {math.Sets.R})

            if len(s1.conditions)     != 0:                                 return FAILED
            if     s1.sureset         != {1, 2, 3}:                         return FAILED
            if     s1.unsureset       != set():                             return FAILED
            if     s1.parents         != {finSure}:                         return FAILED
            if     s1.allParents      != {finSure}:                         return FAILED
            if     s1.type            != math.Set.Type.Finite:              return FAILED
            if     s1.unions          != []:                                return FAILED
            if len(s2.conditions)     != 0:                                 return FAILED
            if     s2.sureset         != set():                             return FAILED
            if     s2.unsureset       != {1, 2, 3}:                         return FAILED
            if     s2.parents         != {finUnsure}:                       return FAILED
            if     s2.allParents      != {finUnsure}:                       return FAILED
            if     s2.type            != math.Set.Type.Finite:              return FAILED
            if     s2.unions          != []:                                return FAILED
            if len(s3.conditions)     != 0:                                 return FAILED
            if     s3.sureset         != set():                             return FAILED
            if     s3.unsureset       != set():                             return FAILED
            if     s3.parents         != {empty}:                           return FAILED
            if     s3.allParents      != {empty}:                           return FAILED
            if     s3.type            != math.Set.Type.Finite:              return FAILED
            if     s3.unions          != []:                                return FAILED
            if len(s4.conditions)     != 0:                                 return FAILED
            if     s4.sureset         != set():                             return FAILED
            if     s4.unsureset       != set():                             return FAILED
            if     s4.parents         != {finUnsure}:                       return FAILED
            if     s4.allParents      != {finUnsure}:                       return FAILED
            if     s4.type            != math.Set.Type.Finite:              return FAILED
            if     s4.unions          != []:                                return FAILED
            if len(s5.conditions)     != 2:                                 return FAILED # 1 from the default of s5 and 1 because desc is its parent
            if     s5.sureset         != set():                             return FAILED
            if     s5.unsureset       != set():                             return FAILED
            if     s5.parents         != {desc}:                            return FAILED
            if     s5.allParents      != {desc}:                            return FAILED
            if     s5.type            != math.Set.Type.Descriptive:         return FAILED
            if     s5.unions          != []:                                return FAILED
            if len(s6.conditions)     != 2:                                 return FAILED # 1 from the construction of s6 and 1 because desc is its parent
            if     s6.sureset         != set():                             return FAILED
            if     s6.unsureset       != set():                             return FAILED
            if     s6.parents         != {desc}:                            return FAILED
            if     s6.allParents      != {desc}:                            return FAILED
            if     s6.type            != math.Set.Type.Descriptive:         return FAILED
            if     s6.unions          != []:                                return FAILED
            if len(s7.conditions)     != 2:                                 return FAILED # 1 from the default of s7 and 1 because descCond is its parent
            if     s7.sureset         != set():                             return FAILED
            if     s7.unsureset       != set():                             return FAILED
            if     s7.parents         != {descCond}:                        return FAILED
            if     s7.allParents      != {descCond}:                        return FAILED
            if     s7.type            != math.Set.Type.Descriptive:         return FAILED
            if     s7.unions          != []:                                return FAILED
            if len(s8.conditions)     != 2:                                 return FAILED # 1 from the construction of s8 and 1 because descCond is its parent
            if     s8.sureset         != set():                             return FAILED
            if     s8.unsureset       != set():                             return FAILED
            if     s8.parents         != {descCond}:                        return FAILED
            if     s8.allParents      != {descCond}:                        return FAILED
            if     s8.type            != math.Set.Type.Descriptive:         return FAILED
            if     s8.unions          != []:                                return FAILED
            if len(s9.conditions)     != 3:                                 return FAILED # 2 from the construction of s9 and 1 because descCond is its parent
            if     s9.sureset         != set():                             return FAILED
            if     s9.unsureset       != set():                             return FAILED
            if     s9.parents         != {descCond}:                        return FAILED
            if     s9.allParents      != {descCond}:                        return FAILED
            if     s9.type            != math.Set.Type.Descriptive:         return FAILED
            if     s9.unions          != []:                                return FAILED
            if len(s10.conditions)    != 2:                                 return FAILED # 1 from the default construction of s10 and 1 because 'descCond | finSure' is listed as a parent
            if     s10.sureset        != {1, 2, 3}:                         return FAILED
            if     s10.unsureset      != set():                             return FAILED
            if     s10.parents        != {union}:                           return FAILED
            if     s10.allParents     != {union}:                           return FAILED
            if     s10.type           != math.Set.Type.Union:               return FAILED
            if     s10.unions         != [descCond]:                        return FAILED
            if len(s11.conditions)    != 3:                                 return FAILED # 1 from the construction of s11 and 2 because 'descCond | finSure' is listed as a parent and R is a parent
            if     s11.sureset        != set():                             return FAILED
            if     s11.unsureset      != {1, 2, 3}:                         return FAILED
            if     s11.parents        != {union, math.Sets.R}:              return FAILED
            if     s11.allParents     != {union, math.Sets.R, math.Sets.C}: return FAILED
            if     s11.type           != math.Set.Type.Union:               return FAILED
            if     s11.unions         != [descCond]:                        return FAILED

            return PASSED
    def MeetsConditions() -> TestResult:
        lT = lambda x: math.Result.TRUE
        lU = lambda x: math.Result.UNSURE
        lF = lambda x: math.Result.FALSE
        lcrash = lambda x: 1/0
        s1  = math.Set(math.Set.Descriptive, [])
        s2  = math.Set(math.Set.Descriptive, [lT])
        s3  = math.Set(math.Set.Descriptive, [lU])
        s4  = math.Set(math.Set.Descriptive, [lF])
        s5  = math.Set(math.Set.Descriptive, [lT, lT])
        s6  = math.Set(math.Set.Descriptive, [lT, lU])
        s7  = math.Set(math.Set.Descriptive, [lT, lF])
        s8  = math.Set(math.Set.Descriptive, [lU, lT])
        s9  = math.Set(math.Set.Descriptive, [lU, lU])
        s10 = math.Set(math.Set.Descriptive, [lU, lF])
        s11 = math.Set(math.Set.Descriptive, [lF, lT])
        s12 = math.Set(math.Set.Descriptive, [lF, lU])
        s13 = math.Set(math.Set.Descriptive, [lF, lF])
        s14 = math.Set(math.Set.Descriptive, [lcrash])

        if s1 ._meetsConditions("dummy") != math.Result.TRUE:   return FAILED
        if s2 ._meetsConditions("dummy") != math.Result.TRUE:   return FAILED
        if s3 ._meetsConditions("dummy") != math.Result.UNSURE: return FAILED
        if s4 ._meetsConditions("dummy") != math.Result.FALSE:  return FAILED
        if s5 ._meetsConditions("dummy") != math.Result.TRUE:   return FAILED
        if s6 ._meetsConditions("dummy") != math.Result.UNSURE: return FAILED
        if s7 ._meetsConditions("dummy") != math.Result.FALSE:  return FAILED
        if s8 ._meetsConditions("dummy") != math.Result.UNSURE: return FAILED
        if s9 ._meetsConditions("dummy") != math.Result.UNSURE: return FAILED
        if s10._meetsConditions("dummy") != math.Result.FALSE:  return FAILED
        if s11._meetsConditions("dummy") != math.Result.FALSE:  return FAILED
        if s12._meetsConditions("dummy") != math.Result.FALSE:  return FAILED
        if s13._meetsConditions("dummy") != math.Result.FALSE:  return FAILED
        if s14._meetsConditions("dummy") != math.Result.FALSE:  return FAILED

        return PASSED
    # TODO: test contains function
    # TODO: test isSubSetOf function
    StringRepresentation = ReprTester([
        (math.Set(math.Set.Empty),                                                                                                      "{}"                                                            ),
        (math.Set(math.Set.Descriptive),                                                                                                "Descriptive Set"                                               ),
        (math.Set({1, 2, 3}),                                                                                                           "{1, 2, 3}"                                                     ),
        (math.Set(set()),                                                                                                               "{}"                                                            ),
        (math.Set({1, 2, 3}, [lambda x: math.Result.UNSURE]),                                                                           "possibly {1, 2, 3}"                                            ),
        (math.Set({1, 2, "3"}, [lambda x: math.Result.UNSURE if type(x)!=int else math.Result.TRUE]),                                   "{1, 2} and possibly {'3'}"                                     ),
        (math.Set(math.Set.Descriptive) | math.Set(math.Set.Descriptive),                                                               "The union of 2 descriptive set(s)"                             ),
        (math.Set(math.Set.Empty) | math.Set(math.Set.Descriptive),                                                                     "The union of 1 descriptive set(s)"                             ),
        (math.Set({1}) | math.Set(math.Set.Descriptive),                                                                                "The union of 1 descriptive set(s) and {1}"                     ),
        (math.Set({1}, [lambda x: math.Result.UNSURE]) | math.Set(math.Set.Descriptive),                                                "The union of 1 descriptive set(s) and possibly {1}"            ),
        (math.Set({1, 2, "3"}, [lambda x: math.Result.UNSURE if type(x)!=int else math.Result.TRUE]) | math.Set(math.Set.Descriptive),  "The union of 1 descriptive set(s), {1, 2} and possibly {'3'}"  ),
    ])
    
    # TODO: split this Empty test over the member function tests when they are written
    def Empty() -> TestResult:
        """Tests the empty set"""
        empty1 = math.Set(math.Set.Empty)
        empty2 = math.Set(math.Set.Empty, [lambda x: False])

        if empty1.contains(1)                                       != math.Result.FALSE:       return FAILED
        if empty1.contains(empty1)                                  != math.Result.FALSE:       return FAILED
        if empty1.contains(empty2)                                  != math.Result.FALSE:       return FAILED
        if empty1.contains(math.Set)                                != math.Result.FALSE:       return FAILED
        if empty1.contains(None)                                    != math.Result.FALSE:       return FAILED
        if empty1.isSubSetOf(1)                                     != math.Result.FALSE:       return FAILED
        if empty1.isSubSetOf(empty1)                                != math.Result.TRUE:        return FAILED
        if empty1.isSubSetOf(empty2)                                != math.Result.TRUE:        return FAILED
        if empty1.isSubSetOf(math.Set(math.Set.Descriptive))        != math.Result.TRUE:        return FAILED
        if empty1.isSubSetOf(math.Set({1,2}))                       != math.Result.TRUE:        return FAILED
        
        if empty2.contains(1)                                       != math.Result.FALSE:       return FAILED
        if empty2.contains(empty1)                                  != math.Result.FALSE:       return FAILED
        if empty2.contains(empty2)                                  != math.Result.FALSE:       return FAILED
        if empty2.contains(math.Set)                                != math.Result.FALSE:       return FAILED
        if empty2.contains(None)                                    != math.Result.FALSE:       return FAILED
        if empty2.isSubSetOf(1)                                     != math.Result.FALSE:       return FAILED
        if empty2.isSubSetOf(empty1)                                != math.Result.TRUE:        return FAILED
        if empty2.isSubSetOf(empty2)                                != math.Result.TRUE:        return FAILED
        if empty2.isSubSetOf(math.Set(math.Set.Descriptive))        != math.Result.TRUE:        return FAILED
        if empty2.isSubSetOf(math.Set({1,2}))                       != math.Result.TRUE:        return FAILED

        return PASSED

class TestMathRelation(Tester):
    """Tests if the Relation class works as intended"""
    def Constructor() -> TestResult:
        """Tests if the __init__ method works as intended"""
        s1 = math.Set({1})
        s2 = math.Set({2})
        lmda = lambda x, y: x+1==y
        rel = math.Relation(s1, s2, lmda)
        if rel.setA     != s1:      return FAILED
        if rel.setB     != s2:      return FAILED
        if rel.relation != lmda:    return FAILED

        try:
            math.Relation("dummy", s2, lmda)
            return FAILED
        except(TypeError): pass
        try:
            math.Relation(s1, "dummy", lmda)
            return FAILED
        except(TypeError): pass
        try:
            math.Relation(s1, s2, "dummy")
            return FAILED
        except(TypeError): pass
        try:
            math.Relation(s1, s2, lambda x: True)
            return FAILED
        except(TypeError): pass

        return PASSED
    def Evaluate() -> TestResult:
        """Tests if the __call__ method works as expected"""
        s135 = math.Set({1, 3, 5})
        s123 = math.Set({1, 2, 3})
        s1 = math.Set({1})
        s246 = math.Set({2, 4, 6})
        s4Cond = math.Set({4}, [lambda x: math.Result.UNSURE])
        s246Cond = math.Set({2, 4, 6}, [lambda x: math.Result.UNSURE])

        r1 = math.Relation(s135, s246,      lambda x, y: 1/0)
        r2 = math.Relation(s135, s246Cond,  lambda x, y: 1/0)
        r3 = math.Relation(s135, s246,      lambda x, y: x+1 == y)
        r4 = math.Relation(s123, s4Cond,    lambda x, y: x+1==y)
        r5 = math.Relation(s135, s246,      lambda x, y: True)
        r6 = math.Relation(s135, s246,      lambda x, y: False)
        r7 = math.Relation(s123, s4Cond,    lambda x, y: True)
        r8 = math.Relation(s123, s4Cond,    lambda x, y: False)
        badboi = math.Relation(s1, s1, lambda x, y: "dummy")

        if r1(1, 2) != math.Result.FALSE:   return FAILED
        if r2(1, 2) != math.Result.FALSE:   return FAILED
        if r3(0, 2) != math.Result.FALSE:   return FAILED
        if r3(1, 0) != math.Result.FALSE:   return FAILED
        if r3(0, 0) != math.Result.FALSE:   return FAILED
        if r3(1, 4) != math.Result.FALSE:   return FAILED
        if r3(1, 2) != math.Result.TRUE:    return FAILED
        if r4(2, 4) != math.Result.FALSE:   return FAILED
        if r4(3, 4) != math.Result.UNSURE:  return FAILED
        if r5(3, 4) != math.Result.TRUE:    return FAILED
        if r6(3, 4) != math.Result.FALSE:   return FAILED
        if r7(3, 4) != math.Result.UNSURE:  return FAILED
        if r8(3, 4) != math.Result.FALSE:   return FAILED

        try:
            badboi(1, 1)
            return FAILED
        except: pass

        return PASSED



# ----- Grouped tests -----------------------------------------------------------------------------
class TestAllMathTests(Tester):
    """Runs all the Math tests"""
    Result = TestMathResult
    SymbolicInfinity = TestMathSymbolicInfinity
    Set = TestMathSet
    Relation = TestMathRelation

# ----- EVERYTHING --------------------------------------------------------------------------------
class TestEVERYTHING(Tester):
    """Runs ALL tests"""
    BHTester = TestBHTester
    MathTests = TestAllMathTests

# TestMathSet()
# TestAllMathTests()
TestEVERYTHING()
