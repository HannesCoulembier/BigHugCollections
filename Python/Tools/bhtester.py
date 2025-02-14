from Tools.bhlog import log, Severity

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
                log(f"Wrongly defined test: test '{testName}' did not return a TestResult. Instead got type {type(res)}", Severity.error)
                continue
            result.succeeded += res.succeeded
            result.testCount += res.testCount
            
        if not silent: log(result, Severity.info)

        return result

class ReprTester:
    """Tests if the __str__ and __repr__ methods work as intended for every pair in a list"""
    def __init__(self, pairs:list[tuple[Any, str]]):
        if type(pairs) != list: raise TypeError("Pairs must be a list of pairs")
        if set(type(pair) for pair in pairs) != {tuple}: raise TypeError("Every pair in pairs must be a tuple")
        if set(len(pair) for pair in pairs) != {2}: raise TypeError("Every pair in pairs must be a tuple with exactly 2 items")
        self.pairs = pairs

    def __call__(self, silent:bool=False) -> TestResult:
        """Tests if the __str__ and __repr__ methods work as intended for every pair in the list"""
        succeeded = 0
        for pair in self.pairs:
            if pair[0].__repr__() == pair[1] and pair[0].__str__() == pair[1]: succeeded += 1
        return TestResult(succeeded=succeeded, testCount=len(self.pairs))
    

class TestBHTester(Tester):
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
        pass # TODO
    class TestReprTester(Tester):
        pass # TODO
