from Tools.bhlog import log, Severity

import inspect
from collections import OrderedDict

class TestResult:
    def __init__(self, succeeded, testCount):
        self.succeeded = succeeded
        self.testCount = testCount

class Tester:
    """An instance of a child of this class can be called. When called, it will execute all methods associated with the child that take no parameters and don't start with a '_'"""
    def __new__(cls, silent=False) -> TestResult:
        sig1 = OrderedDict([])
        sig2 = OrderedDict([('silent', inspect.Parameter("silent",inspect.Parameter.POSITIONAL_OR_KEYWORD, default=False))])
        testNames = [method for method in cls.__dict__ if callable(getattr(cls, method)) and not method.startswith("_") and (inspect.signature(getattr(cls, method)).parameters == sig1 or inspect.signature(getattr(cls, method)).parameters == sig2)]

        succeeded = 0
        testCount = 0
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
                testCount += 1
                continue

            if type(res) != TestResult:
                log(f"Wrongly defined test: test '{testName}' did not return a TestResult. Instead got type {type(res)}", Severity.error)
                continue
            succeeded += res.succeeded
            testCount += res.testCount
            
        if not silent: log(f"{succeeded} out of {testCount} tests succeeded ({round(100*succeeded/testCount, 2)}%)", Severity.info)

        return TestResult(succeeded, testCount)
