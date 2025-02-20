if __name__ == "__main__":  from       bhlog import log, Severity
else:                       from Tools.bhlog import log, Severity

import inspect
from typing import Any
from collections import OrderedDict

class TestResult:
    def __init__(self, succeeded:int, testCount:int):
        if type(succeeded) != int: raise TypeError("succeeded must be an integer")
        if type(testCount) != int: raise TypeError("testCount must be an integer")
        self.succeeded = succeeded
        self.testCount = testCount
    def __repr__(self) -> str:
        percent = 100.0
        if self.testCount != 0:
            percent = round(100*self.succeeded/self.testCount, 2)
        return f"{self.succeeded} out of {self.testCount} tests succeeded ({percent}%)"
    def __eq__(self, x:Any) -> bool:
        if type(x) != TestResult: return NotImplemented
        return self.succeeded == x.succeeded and self.testCount == x.testCount

PASSED = TestResult(1, 1)
FAILED = TestResult(0, 1)

class Tester:
    """An instance of a child of this class can be called. When called, it will execute all methods associated with the child that take no parameters and don't start with a '_'"""
    def __new__(cls, silent:bool=False) -> TestResult:
        sig1 = OrderedDict([])
        sig2 = OrderedDict([('silent', inspect.Parameter("silent",inspect.Parameter.POSITIONAL_OR_KEYWORD, default=False, annotation=bool))])
        testNames = [method for method in cls.__dict__ if callable(getattr(cls, method)) and not method.startswith("_") and (inspect.signature(getattr(cls, method)).parameters == sig1 or inspect.signature(getattr(cls, method)).parameters == sig2)]
        
        result = TestResult(succeeded = 0, testCount = 0)
        for testName in testNames:
            if not silent: log(f"Testing: '{testName}'", Severity.info)
            method = getattr(cls, testName)
            if method.__doc__ == None or method.__doc__ == "": log(f"Test '{testName}' has no docstring", Severity.warn)

            try:
                if inspect.signature(method).parameters == sig1:
                    res = method()
                else:
                    res = method(silent=True)
            except:
                if not silent: log(f"Crashed", Severity.warn)
                result.testCount += 1
                continue

            if type(res) != TestResult:
                if not silent: log(f"Wrongly defined test: test '{testName}' did not return a TestResult. Instead got type {type(res)}", Severity.error)
                result.testCount += 1
                continue
            result.succeeded += res.succeeded
            result.testCount += res.testCount
            
        if not silent: log(result, Severity.info)

        return result

class ReprTester:
    """Tests if the __str__ and __repr__ methods work as intended for every pair in a list"""
    def __init__(self, pairs:list[tuple[Any, str]]):
        if type(pairs) != list: raise TypeError("Pairs must be a list of pairs")
        if len(pairs) != 0:
            if set(type(pair) for pair in pairs) != {tuple}: raise TypeError("Every pair in pairs must be a tuple")
            if set(len(pair) for pair in pairs) != {2}: raise ValueError("Every pair in pairs must be a tuple with exactly 2 items")
        self.pairs = pairs

    def __call__(self) -> TestResult:
        """Tests if the __str__ and __repr__ methods work as intended for every pair in the list"""
        succeeded = 0
        for pair in self.pairs:
            try:
                if pair[0].__repr__() == pair[1] and pair[0].__str__() == pair[1]: succeeded += 1
            except:
                continue
        return TestResult(succeeded=succeeded, testCount=len(self.pairs))
    

class TestBHTester(Tester):
    """Tests all bhtester related objects"""
    class TestTestResult(Tester):
        """Tests if the TestResult class works as intended"""
        def Construct() -> TestResult:
            """Tests if the __init__ method works as intended"""
            R1 = TestResult(20, 70)
            if R1.succeeded != 20: return FAILED
            if R1.testCount != 70: return FAILED
            try:
                R2 = TestResult("20", 40)
                return FAILED
            except(TypeError): pass
            try:
                R2 = TestResult(20, '40')
                return FAILED
            except(TypeError): pass
            return PASSED        
        def EqualityOperator() -> TestResult:
            """Tests if the __eq__ method works as intended"""
            if TestResult(0, 0) != TestResult(0, 0): return FAILED
            if TestResult(1, 0) != TestResult(1, 0): return FAILED
            if TestResult(0, 1) != TestResult(0, 1): return FAILED
            if TestResult(1, 1) != TestResult(1, 1): return FAILED
            if TestResult(0, 2) == TestResult(0, 0): return FAILED
            if TestResult(2, 0) == TestResult(0, 0): return FAILED
            if TestResult(2, 2) == TestResult(0, 0): return FAILED
            
            if TestResult(2, 1).__eq__("dummy") != NotImplemented: return FAILED
            if TestResult(2, 1).__eq__(True)    != NotImplemented: return FAILED

            return PASSED
        StringRepresentation = ReprTester([
            (TestResult(0, 0), "0 out of 0 tests succeeded (100.0%)"),
            (TestResult(1, 0), "1 out of 0 tests succeeded (100.0%)"),
            (TestResult(1, 1), "1 out of 1 tests succeeded (100.0%)"),
            (TestResult(1, 2), "1 out of 2 tests succeeded (50.0%)"),
            (TestResult(0, 2), "0 out of 2 tests succeeded (0.0%)"),
            (TestResult(1, 9), "1 out of 9 tests succeeded (11.11%)"),
            (TestResult(2, 3), "2 out of 3 tests succeeded (66.67%)"),
        ])
    class TestTester(Tester):
        """Tests if the Tester class works as intended

        Important! This Tester does not test the logging capabilities to prevent clutter, this means all Tester instances are called using silent=True
        """
        def BaseClass() -> TestResult:
            """Tests if the base class works as intended"""
            if Tester(silent=True) != TestResult(0, 0): return FAILED # Tester should have no tests of its own, because they would be inherited by the children
            return PASSED
        def MethodDetection() -> TestResult:
            """Tests if the test methods of a child are correctly detected"""
            class Child1(Tester):
                def m1() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
            class Child2(Tester):
                def _m1() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
            class Child3(Tester):
                m1 = PASSED
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
            class Child4(Tester):
                def m1(incorrect_argument_name) -> TestResult:
                    """This is a testmethod"""
                    return PASSED
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
            class Child4(Tester):
                def m1(silent) -> TestResult: # Has no default value nor type annotation
                    """This is a testmethod"""
                    return PASSED
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
            class Child5(Tester):
                def m1(silent=False) -> TestResult: # Has no type annotation
                    """This is a testmethod"""
                    return PASSED
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
            class Child6(Tester):
                def m1(silent:bool) -> TestResult: # Has no default value
                    """This is a testmethod"""
                    return PASSED
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return PASSED
            class Child7(Tester):
                def m1(silent:bool = True) -> TestResult: # Has incorrect default value
                    """This is a testmethod"""
                    return PASSED
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return PASSED

            # Some special cases
            class Child8(Tester):
                class Baby(Tester):
                    """This is a test class"""
                    def m() -> TestResult:
                        """This is a test method"""
                        return PASSED
            class Child9(Tester):
                StringBaby = ReprTester([
                    (True, "True")
                ])
            
            if Child1(silent=True) != TestResult(2, 2): return FAILED
            if Child2(silent=True) != TestResult(1, 1): return FAILED
            if Child3(silent=True) != TestResult(1, 1): return FAILED
            if Child4(silent=True) != TestResult(1, 1): return FAILED
            if Child5(silent=True) != TestResult(1, 1): return FAILED
            if Child6(silent=True) != TestResult(1, 1): return FAILED
            if Child7(silent=True) != TestResult(1, 1): return FAILED

            if Child8(silent=True) != TestResult(1, 1): return FAILED
            if Child9(silent=True) != TestResult(1, 1): return FAILED

            return PASSED
        def CrashHandling() -> TestResult:
            """Tests if crashes are handled correctly"""
            class Child1(Tester):
                def crash() -> TestResult:
                    """This is a testmethod"""
                    raise Exception
            class Child2(Tester):
                def crash() -> TestResult:
                    """This is a testmethod"""
                    raise Exception("Error message")
            class Child3(Tester):
                def crash() -> TestResult:
                    """This is a testmethod"""
                    raise BaseException
            class Child4(Tester):
                def crash() -> TestResult:
                    """This is a testmethod"""
                    raise BaseException("Error message")
            class Child5(Tester):
                def crash1() -> TestResult:
                    """This is a testmethod"""
                    raise Exception
                def crash2() -> TestResult:
                    """This is a testmethod"""
                    raise Exception

            if Child1(silent=True) != TestResult(0, 1): return FAILED
            if Child2(silent=True) != TestResult(0, 1): return FAILED
            if Child3(silent=True) != TestResult(0, 1): return FAILED
            if Child4(silent=True) != TestResult(0, 1): return FAILED
            if Child5(silent=True) != TestResult(0, 2): return FAILED

            return PASSED
        def MethodEvaluation() -> TestResult:
            """Tests if the result of a test method is processed correctly"""
            class Child1(Tester):
                def m() -> TestResult:
                    """This is a testmethod"""
                    return TestResult(0, 0)
            class Child2(Tester):
                def m() -> TestResult:
                    """This is a testmethod"""
                    return TestResult(1, 2)
            class Child3(Tester):
                def m() -> TestResult:
                    """This is a testmethod"""
                    return "dummy"
            class Child4(Tester):
                def m1() -> TestResult:
                    """This is a testmethod"""
                    return TestResult(1, 2)
                def m2() -> TestResult:
                    """This is a testmethod"""
                    return "dummy"
                def m3() -> TestResult:
                    """This is a testmethod"""
                    return TestResult(1, 2)
                def m4() -> TestResult:
                    """This is a testmethod"""
                    return TestResult(0, 0)

            if Child1(silent=True) != TestResult(0, 0): return FAILED
            if Child2(silent=True) != TestResult(1, 2): return FAILED
            if Child3(silent=True) != TestResult(0, 1): return FAILED
            if Child4(silent=True) != TestResult(2, 5): return FAILED

            return PASSED
    class TestReprTester(Tester):
        """Tests if the ReprTester class works as intended"""
        def Constructor() -> TestResult:
            """Tests if the __init__ method works as intended"""
            if ReprTester([]).pairs                 != []:                  return FAILED
            if ReprTester([(1, 2), (3, 4)]).pairs   != [(1, 2), (3, 4)]:    return FAILED

            try:
                ReprTester("dummy")
                return FAILED
            except(TypeError): pass
            try:
                ReprTester(["dummy", (3, 4)])
                return FAILED
            except(TypeError): pass
            try:
                ReprTester([(1,), (2, 3)])
                return FAILED
            except(ValueError): pass
            
            return PASSED
        def Execution() -> TestResult:
            """Tests if the class works correctly"""
            if ReprTester([])()                                 != TestResult(0, 0): return FAILED
            if ReprTester([(True, "True")])()                   != TestResult(1, 1): return FAILED
            if ReprTester([(True, "True"), (False, "False")])() != TestResult(2, 2): return FAILED
            if ReprTester([(True, True)])()                     != TestResult(0, 1): return FAILED
            if ReprTester([(True, True), (False, "False")])()   != TestResult(1, 2): return FAILED

            class WhyWouldThisExist:
                def __repr__(self) -> str:
                    raise Exception
            
            if ReprTester([(WhyWouldThisExist(), "dummy")])()   != TestResult(0, 1): return FAILED

            return PASSED

if __name__ == "__main__":
    TestBHTester()