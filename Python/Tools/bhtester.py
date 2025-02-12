from Tools.bhlog import log, Severity

import inspect

class TestResult:
    def __init__(self, succeeded, testCount):
        self.succeeded = succeeded
        self.testCount = testCount

class Tester:
    """An instance of a child of this class can be called. When called, it will execute all methods associated with the child that take no parameters and don't start with a '_'"""
    def __new__(cls, silent=False) -> TestResult:
        testNames = [method for method in cls.__dict__ if callable(getattr(cls, method)) and not method.startswith("_") and len(inspect.signature(getattr(cls, method)).parameters) == 0]
        succeeded = 0
        testCount = 0
        for testName in testNames:
            if not silent: log(f"Testing: '{testName}'", Severity.info)
            try:
                res = getattr(cls, testName)()
            except:
                if not silent: log(f"Crashed", Severity.warn)
                testCount += 1
                continue

            if type(res) != TestResult:
                log(f"Wrongly defined test: test '{testName}' did not return a TestResult. Instead got type {type(res)}", Severity.error)
                continue
            succeeded += res.succeeded
            testCount += res.testCount
            
        if not silent: log(f"{succeeded} out of {testCount} tests succeeded ({round(100*succeeded/testCount, 2)}%)", Severity.info)

        return TestResult(succeeded, testCount)
