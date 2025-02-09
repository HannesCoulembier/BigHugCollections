from Tools.bhlog import log, Severity

import inspect

class Tester:
    """An instance of a child of this class can be called. When called, it will execute all methods associated with the child that take no parameters and don't start with a '_'"""
    def __new__(cls, silent=False) -> bool:
        testNames = [method for method in cls.__dict__ if callable(getattr(cls, method)) and not method.startswith("_") and len(inspect.signature(getattr(cls, method)).parameters) == 0]
        succeeded = 0
        for testName in testNames:
            if not silent: log(f"Testing: '{testName}'", Severity.info)
            try:
                res = getattr(cls, testName)()
            except:
                if not silent: log(f"Crashed", Severity.warn)
                continue

            if type(res) != bool:
                if not silent: log(f"Wrongly defined test: test '{testName}' did not return a boolean. Instead got type {type(res)}", Severity.error)
                continue
            if res: succeeded += 1
            if not silent: log("Succeeded" if res else "Failed", Severity.info)
            
        if not silent: log(f"{succeeded} out of {len(testNames)} tests succeeded ({round(100*succeeded/len(testNames), 2)}%)", Severity.info)

        return succeeded == len(testNames)
