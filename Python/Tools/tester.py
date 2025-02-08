from Tools.log import log, Severity

import inspect

class Tester:
    """An instance of a child of this class can be called. When called, it will execute all methods associated with the child that take no parameters and don't start with a '_'"""
    def __init__(self):
        testNames = [method for method in type(self).__dict__ if callable(getattr(type(self), method)) and not method.startswith("_") and len(inspect.signature(getattr(type(self), method)).parameters) == 0]
        succeeded = 0
        for testName in testNames:
            log(f"Testing: '{testName}'", Severity.info)
            try:
                res = getattr(type(self), testName)()
            except:
                log(f"Crashed", Severity.warn)
                continue

            if type(res) != bool:
                log(f"Wrongly defined test: test '{testName}' did not return a boolean. Instead got type {type(res)}", Severity.error)
                continue
            if res: succeeded += 1
            log("Succeeded" if res else "Failed", Severity.info)
        log(f"{succeeded} out of {len(testNames)} tests succeeded ({round(100*succeeded/len(testNames), 2)}%)", Severity.info)
