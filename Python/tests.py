from Tools.bhtester import Tester, ReprTester, TestResult, PASSED, FAILED, TestBHTester, EqualityTester

import Tools.bhmath as math

# ----- All individual tests ----------------------------------------------------------------------
class TestMathResult(Tester):
    """Tests if the Result class works as intended"""
    def ResultToBool() -> TestResult:
        """Tests if the __bool__ method works as intended"""
        toBool = EqualityTester([
            (bool(math.Result.TRUE), True),     # Converting Result.TRUE  should yield True
            (bool(math.Result.FALSE), False),   # Converting Result.FALSE should yield False
        ])
        res = toBool()
        try:
            U = bool(math.Result.UNSURE) # Trying to convert Result.UNSURE should raise a TypeError
        except NotImplementedError:
            return PASSED + res
        return FAILED + res # This will run when either the conversion in the try block succeeded or the wrong exception was thrown
    StringRepresentation = ReprTester([
        (math.Result.TRUE,      "TRUE"),
        (math.Result.UNSURE,    "UNSURE"),
        (math.Result.FALSE,     "FALSE"),
    ])
    # Tests if the __eq__ method works as intended
    EqualityOperator = EqualityTester([
        ((math.Result.FALSE  == math.Result.FALSE),  True),
        ((math.Result.FALSE  == math.Result.UNSURE), False),
        ((math.Result.FALSE  == math.Result.TRUE),   False),
        ((math.Result.UNSURE == math.Result.FALSE),  False),
        ((math.Result.UNSURE == math.Result.UNSURE), True),
        ((math.Result.UNSURE == math.Result.TRUE),   False),
        ((math.Result.TRUE   == math.Result.FALSE),  False),
        ((math.Result.TRUE   == math.Result.UNSURE), False),
        ((math.Result.TRUE   == math.Result.TRUE),   True),

        ((math.Result.FALSE == False),  True),
        ((math.Result.FALSE == True),   False),
        ((math.Result.UNSURE == False), False),
        ((math.Result.UNSURE == True),  False),
        ((math.Result.TRUE  == False),  False),
        ((math.Result.TRUE  == True),   True),
        ((False == math.Result.FALSE),  True),
        ((True  == math.Result.FALSE),  False),
        ((False == math.Result.UNSURE), False),
        ((True  == math.Result.UNSURE), False),
        ((False == math.Result.TRUE),   False),
        ((True  == math.Result.TRUE),   True),

        (math.Result.FALSE.__eq__("Dummy"),  NotImplemented),
        (math.Result.UNSURE.__eq__("Dummy"), NotImplemented),
        (math.Result.TRUE.__eq__("Dummy"),   NotImplemented),
    ])
    # Tests if the __or__ and __ror__ methods work as intended
    OrOperator = EqualityTester([
        ((math.Result.FALSE  | math.Result.FALSE),  math.Result.FALSE),
        ((math.Result.FALSE  | math.Result.UNSURE), math.Result.UNSURE),
        ((math.Result.FALSE  | math.Result.TRUE),   math.Result.TRUE),
        ((math.Result.UNSURE | math.Result.FALSE),  math.Result.UNSURE),
        ((math.Result.UNSURE | math.Result.UNSURE), math.Result.UNSURE),
        ((math.Result.UNSURE | math.Result.TRUE),   math.Result.TRUE),
        ((math.Result.TRUE   | math.Result.FALSE),  math.Result.TRUE),
        ((math.Result.TRUE   | math.Result.UNSURE), math.Result.TRUE),
        ((math.Result.TRUE   | math.Result.TRUE),   math.Result.TRUE),
        ((math.Result.FALSE  | False),  math.Result.FALSE),
        ((math.Result.FALSE  | True),   math.Result.TRUE),
        ((math.Result.UNSURE | False),  math.Result.UNSURE),
        ((math.Result.UNSURE | True),   math.Result.TRUE),
        ((math.Result.TRUE   | False),  math.Result.TRUE),
        ((math.Result.TRUE   | True),   math.Result.TRUE),
        ((False | math.Result.FALSE),   math.Result.FALSE),
        ((True  | math.Result.FALSE),   math.Result.TRUE),
        ((False | math.Result.UNSURE),  math.Result.UNSURE),
        ((True  | math.Result.UNSURE),  math.Result.TRUE),
        ((False | math.Result.TRUE),    math.Result.TRUE),
        ((True  | math.Result.TRUE),    math.Result.TRUE),

        (math.Result.FALSE.__or__("Dummy"),     NotImplemented),
        (math.Result.UNSURE.__or__("Dummy"),    NotImplemented),
        (math.Result.TRUE.__or__("Dummy"),      NotImplemented),
        (math.Result.FALSE.__ror__("Dummy"),    NotImplemented),
        (math.Result.UNSURE.__ror__("Dummy"),   NotImplemented),
        (math.Result.TRUE.__ror__("Dummy"),     NotImplemented),
    ])
    # Tests if the __and__ and __rand__ methods work as intended
    AndOperator = EqualityTester([
        ((math.Result.FALSE  & math.Result.FALSE),  math.Result.FALSE),
        ((math.Result.FALSE  & math.Result.UNSURE), math.Result.FALSE),
        ((math.Result.FALSE  & math.Result.TRUE),   math.Result.FALSE),
        ((math.Result.UNSURE & math.Result.FALSE),  math.Result.FALSE),
        ((math.Result.UNSURE & math.Result.UNSURE), math.Result.UNSURE),
        ((math.Result.UNSURE & math.Result.TRUE),   math.Result.UNSURE),
        ((math.Result.TRUE   & math.Result.FALSE),  math.Result.FALSE),
        ((math.Result.TRUE   & math.Result.UNSURE), math.Result.UNSURE),
        ((math.Result.TRUE   & math.Result.TRUE),   math.Result.TRUE),
        ((math.Result.FALSE  & False),  math.Result.FALSE),
        ((math.Result.FALSE  & True),   math.Result.FALSE),
        ((math.Result.UNSURE & False),  math.Result.FALSE),
        ((math.Result.UNSURE & True),   math.Result.UNSURE),
        ((math.Result.TRUE   & False),  math.Result.FALSE),
        ((math.Result.TRUE   & True),   math.Result.TRUE),
        ((False & math.Result.FALSE),   math.Result.FALSE),
        ((True  & math.Result.FALSE),   math.Result.FALSE),
        ((False & math.Result.UNSURE),  math.Result.FALSE),
        ((True  & math.Result.UNSURE),  math.Result.UNSURE),
        ((False & math.Result.TRUE),    math.Result.FALSE),
        ((True  & math.Result.TRUE),    math.Result.TRUE),

        (math.Result.FALSE.__and__("Dummy"),    NotImplemented),
        (math.Result.UNSURE.__and__("Dummy"),   NotImplemented),
        (math.Result.TRUE.__and__("Dummy"),     NotImplemented),
        (math.Result.FALSE.__rand__("Dummy"),   NotImplemented),
        (math.Result.UNSURE.__rand__("Dummy"),  NotImplemented),
        (math.Result.TRUE.__rand__("Dummy"),    NotImplemented),
    ])

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

            test = EqualityTester([
                (empty1.conditions,   []),
                (empty1.sureset,      set()),
                (empty1.unsureset,    set()),
                (empty1.parents,      set()),
                (empty1.allParents,   set()),
                (empty1.type,         math.Set.Type.Finite),
                (empty1.unions,       []),
            
                (empty2.conditions,   []),
                (empty2.sureset,      set()),
                (empty2.unsureset,    set()),
                (empty2.parents,      set()),
                (empty2.allParents,   set()),
                (empty2.type,         math.Set.Type.Finite),
                (empty2.unions,       []),
            ])
            return test()
        def Descriptive() -> TestResult:
            """Tests Descriptive Sets constructors"""
            desc1 = math.Set(math.Set.Descriptive)
            desc2 = math.Set(math.Set.Descriptive, [lambda x: math.Result.UNSURE])
            desc3 = math.Set(math.Set.Descriptive, parents={math.Sets.R})
            desc4 = math.Set(math.Set.Descriptive, [lambda x: math.Result.UNSURE], parents={math.Sets.R})

            test = EqualityTester([
                (len(desc1.conditions),  1),
                (    desc1.sureset,      set()),
                (    desc1.unsureset,    set()),
                (    desc1.parents,      set()),
                (    desc1.allParents,   set()),
                (    desc1.type,         math.Set.Type.Descriptive),
                (    desc1.unions,       []),
                (len(desc2.conditions),  1),
                (    desc2.sureset,      set()),
                (    desc2.unsureset,    set()),
                (    desc2.parents,      set()),
                (    desc2.allParents,   set()),
                (    desc2.type,         math.Set.Type.Descriptive),
                (    desc2.unions,       []),
                (len(desc3.conditions),  2),
                (    desc3.sureset,      set()),
                (    desc3.unsureset,    set()),
                (    desc3.parents,      {math.Sets.R}),
                (    desc3.allParents,   {math.Sets.R, math.Sets.C}),
                (    desc3.type,         math.Set.Type.Descriptive),
                (    desc3.unions,       []),
                (len(desc4.conditions),  2),
                (    desc4.sureset,      set()),
                (    desc4.unsureset,    set()),
                (    desc4.parents,      {math.Sets.R}),
                (    desc4.allParents,   {math.Sets.R, math.Sets.C}),
                (    desc4.type,         math.Set.Type.Descriptive),
                (    desc4.unions,       []),
            ])

            return test()
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

            test = EqualityTester([
                (len(lst1.conditions),    0),
                (    lst1.sureset,        set()),
                (    lst1.unsureset,      set()),
                (    lst1.parents,        set()),
                (    lst1.allParents,     set()),
                (    lst1.type,           math.Set.Type.Finite),
                (    lst1.unions,         []),
                (len(lst2.conditions),    0),
                (    lst2.sureset,        {1, 2, "3"}),
                (    lst2.unsureset,      set()),
                (    lst2.parents,        set()),
                (    lst2.allParents,     set()),
                (    lst2.type,           math.Set.Type.Finite),
                (    lst2.unions,         []),
                (len(lst3.conditions),    0),
                (    lst3.sureset,        set()),
                (    lst3.unsureset,      set()),
                (    lst3.parents,        set()),
                (    lst3.allParents,     set()),
                (    lst3.type,           math.Set.Type.Finite),
                (    lst3.unions,         []),
                (len(lst4.conditions),    0),
                (    lst4.sureset,        set()),
                (    lst4.unsureset,      {1, 2, "3"}),
                (    lst4.parents,        set()),
                (    lst4.allParents,     set()),
                (    lst4.type,           math.Set.Type.Finite),
                (    lst4.unions,         []),
                (len(lst5.conditions),    0),
                (    lst5.sureset,        {1, 2, "3"}),
                (    lst5.unsureset,      set()),
                (    lst5.parents,        set()),
                (    lst5.allParents,     set()),
                (    lst5.type,           math.Set.Type.Finite),
                (    lst5.unions,         []),
                (len(lst6.conditions),    0),
                (    lst6.sureset,        {1, 2}),
                (    lst6.unsureset,      set()),
                (    lst6.parents,        {math.Sets.R}),
                (    lst6.allParents,     {math.Sets.R, math.Sets.C}),
                (    lst6.type,           math.Set.Type.Finite),
                (    lst6.unions,         []),
                (len(lst7.conditions),    0),
                (    lst7.sureset,        set()),
                (    lst7.unsureset,      {1, 2}),
                (    lst7.parents,        {math.Sets.R}),
                (    lst7.allParents,     {math.Sets.R, math.Sets.C}),
                (    lst7.type,           math.Set.Type.Finite),
                (    lst7.unions,         []),
                (len(set1.conditions),    0),
                (    set1.sureset,        set()),
                (    set1.unsureset,      set()),
                (    set1.parents,        set()),
                (    set1.allParents,     set()),
                (    set1.type,           math.Set.Type.Finite),
                (    set1.unions,         []),
                (len(set2.conditions),    0),
                (    set2.sureset,        {1, 2, "3"}),
                (    set2.unsureset,      set()),
                (    set2.parents,        set()),
                (    set2.allParents,     set()),
                (    set2.type,           math.Set.Type.Finite),
                (    set2.unions,         []),
                (len(set3.conditions),    0),
                (    set3.sureset,        set()),
                (    set3.unsureset,      set()),
                (    set3.parents,        set()),
                (    set3.allParents,     set()),
                (    set3.type,           math.Set.Type.Finite),
                (    set3.unions,         []),
                (len(set4.conditions),    0),
                (    set4.sureset,        set()),
                (    set4.unsureset,      {1, 2, "3"}),
                (    set4.parents,        set()),
                (    set4.allParents,     set()),
                (    set4.type,           math.Set.Type.Finite),
                (    set4.unions,         []),
                (len(set5.conditions),    0),
                (    set5.sureset,        {1, 2, "3"}),
                (    set5.unsureset,      set()),
                (    set5.parents,        set()),
                (    set5.allParents,     set()),
                (    set5.type,           math.Set.Type.Finite),
                (    set5.unions,         []),
                (len(set6.conditions),    0),
                (    set6.sureset,        {1, 2}),
                (    set6.unsureset,      set()),
                (    set6.parents,        {math.Sets.R}),
                (    set6.allParents,     {math.Sets.R, math.Sets.C}),
                (    set6.type,           math.Set.Type.Finite),
                (    set6.unions,         []),
                (len(set7.conditions),    0),
                (    set7.sureset,        set()),
                (    set7.unsureset,      {1, 2}),
                (    set7.parents,        {math.Sets.R}),
                (    set7.allParents,     {math.Sets.R, math.Sets.C}),
                (    set7.type,           math.Set.Type.Finite),
                (    set7.unions,         []),
            ])

            return test()
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

            test = EqualityTester([
                (len(s1.conditions),     0),
                (    s1.sureset,         {1, 2, 3}),
                (    s1.unsureset,       set()),
                (    s1.parents,         {finSure}),
                (    s1.allParents,      {finSure}),
                (    s1.type,            math.Set.Type.Finite),
                (    s1.unions,          []),
                (len(s2.conditions),     0),
                (    s2.sureset,         set()),
                (    s2.unsureset,       {1, 2, 3}),
                (    s2.parents,         {finUnsure}),
                (    s2.allParents,      {finUnsure}),
                (    s2.type,            math.Set.Type.Finite),
                (    s2.unions,          []),
                (len(s3.conditions),     0),
                (    s3.sureset,         set()),
                (    s3.unsureset,       set()),
                (    s3.parents,         {empty}),
                (    s3.allParents,      {empty}),
                (    s3.type,            math.Set.Type.Finite),
                (    s3.unions,          []),
                (len(s4.conditions),     0),
                (    s4.sureset,         set()),
                (    s4.unsureset,       set()),
                (    s4.parents,         {finUnsure}),
                (    s4.allParents,      {finUnsure}),
                (    s4.type,            math.Set.Type.Finite),
                (    s4.unions,          []),
                (len(s5.conditions),     2), # 1 from the default of s5 and 1 because desc is its parent
                (    s5.sureset,         set()),
                (    s5.unsureset,       set()),
                (    s5.parents,         {desc}),
                (    s5.allParents,      {desc}),
                (    s5.type,            math.Set.Type.Descriptive),
                (    s5.unions,          []),
                (len(s6.conditions),     2), # 1 from the construction of s6 and 1 because desc is its parent
                (    s6.sureset,         set()),
                (    s6.unsureset,       set()),
                (    s6.parents,         {desc}),
                (    s6.allParents,      {desc}),
                (    s6.type,            math.Set.Type.Descriptive),
                (    s6.unions,          []),
                (len(s7.conditions),     2), # 1 from the default of s7 and 1 because descCond is its parent
                (    s7.sureset,         set()),
                (    s7.unsureset,       set()),
                (    s7.parents,         {descCond}),
                (    s7.allParents,      {descCond}),
                (    s7.type,            math.Set.Type.Descriptive),
                (    s7.unions,          []),
                (len(s8.conditions),     2), # 1 from the construction of s8 and 1 because descCond is its parent
                (    s8.sureset,         set()),
                (    s8.unsureset,       set()),
                (    s8.parents,         {descCond}),
                (    s8.allParents,      {descCond}),
                (    s8.type,            math.Set.Type.Descriptive),
                (    s8.unions,          []),
                (len(s9.conditions),     3), # 2 from the construction of s9 and 1 because descCond is its parent
                (    s9.sureset,         set()),
                (    s9.unsureset,       set()),
                (    s9.parents,         {descCond}),
                (    s9.allParents,      {descCond}),
                (    s9.type,            math.Set.Type.Descriptive),
                (    s9.unions,          []),
                (len(s10.conditions),    2), # 1 from the default construction of s10 and 1 because 'descCond | finSure' is listed as a parent
                (    s10.sureset,        {1, 2, 3}),
                (    s10.unsureset,      set()),
                (    s10.parents,        {union}),
                (    s10.allParents,     {union}),
                (    s10.type,           math.Set.Type.Union),
                (    s10.unions,         [descCond]),
                (len(s11.conditions),    3), # 1 from the construction of s11 and 2 because 'descCond | finSure' is listed as a parent and R is a parent
                (    s11.sureset,        set()),
                (    s11.unsureset,      {1, 2, 3}),
                (    s11.parents,        {union, math.Sets.R}),
                (    s11.allParents,     {union, math.Sets.R, math.Sets.C}),
                (    s11.type,           math.Set.Type.Union),
                (    s11.unions,         [descCond]),
            ])

            return test()
    def MeetsConditions() -> TestResult:
        """Tests if the _meetsConditions method works as intended"""
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

        test = EqualityTester([
            (s1 ._meetsConditions("dummy"), math.Result.TRUE),
            (s2 ._meetsConditions("dummy"), math.Result.TRUE),
            (s3 ._meetsConditions("dummy"), math.Result.UNSURE),
            (s4 ._meetsConditions("dummy"), math.Result.FALSE),
            (s5 ._meetsConditions("dummy"), math.Result.TRUE),
            (s6 ._meetsConditions("dummy"), math.Result.UNSURE),
            (s7 ._meetsConditions("dummy"), math.Result.FALSE),
            (s8 ._meetsConditions("dummy"), math.Result.UNSURE),
            (s9 ._meetsConditions("dummy"), math.Result.UNSURE),
            (s10._meetsConditions("dummy"), math.Result.FALSE),
            (s11._meetsConditions("dummy"), math.Result.FALSE),
            (s12._meetsConditions("dummy"), math.Result.FALSE),
            (s13._meetsConditions("dummy"), math.Result.FALSE),
            (s14._meetsConditions("dummy"), math.Result.FALSE),
        ])

        return test()
    # TODO: test contains function
    # TODO: test isSubSetOf function
    # TODO: test union creation
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

        test = EqualityTester([
            (r1(1, 2), math.Result.FALSE),
            (r2(1, 2), math.Result.FALSE),
            (r3(0, 2), math.Result.FALSE),
            (r3(1, 0), math.Result.FALSE),
            (r3(0, 0), math.Result.FALSE),
            (r3(1, 4), math.Result.FALSE),
            (r3(1, 2), math.Result.TRUE),
            (r4(2, 4), math.Result.FALSE),
            (r4(3, 4), math.Result.UNSURE),
            (r5(3, 4), math.Result.TRUE),
            (r6(3, 4), math.Result.FALSE),
            (r7(3, 4), math.Result.UNSURE),
            (r8(3, 4), math.Result.FALSE),
        ])

        res = test()
        try:
            badboi(1, 1)
            return FAILED + res
        except: pass

        return PASSED + res



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
