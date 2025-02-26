from Tools.bhtester import Tester, ReprTester, TestResult, PASSED, FAILED, TestBHTester, EqualityTester

import Tools.bhmath as math

from numbers import Real

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
        test = EqualityTester([
            ((math.SymbolicInfinity(False) == math.SymbolicInfinity(False)),  True),
            ((math.SymbolicInfinity(False) == math.SymbolicInfinity(True)),   False),
            ((math.SymbolicInfinity(True)  == math.SymbolicInfinity(False)),  False),
            ((math.SymbolicInfinity(True)  == math.SymbolicInfinity(True)),   True),
        
            (math.SymbolicInfinity(False).__eq__("Dummy"), NotImplemented),
            (math.SymbolicInfinity(True).__eq__("Dummy"),  NotImplemented),
        ])
        return test()
    def NegationOperator() -> TestResult:
        """Tests if the __neg__ method works as intended"""
        test = EqualityTester([
            (-math.SymbolicInfinity(False), math.SymbolicInfinity(True)),
            (-math.SymbolicInfinity(True),  math.SymbolicInfinity(False)),    
        ])
        return test()
    def ComparisonOperators() -> TestResult:
        """Tests if the __gt__, __lt__, __ge__ and __le__ methods work as intended"""
        P = math.SymbolicInfinity()
        N = -P

        test = EqualityTester([
            ((N >  N), False),
            ((N >  P), False),
            ((P >  N), True),
            ((P >  P), False),
            ((N >= N), True),
            ((N >= P), False),
            ((P >= N), True),
            ((P >= P), True),
            ((N <  N), False),
            ((N <  P), True),
            ((P <  N), False),
            ((P <  P), False),
            ((N <= N), True),
            ((N <= P), True),
            ((P <= N), False),
            ((P <= P), True),

            ((N >  0), False),
            ((P >  0), True),
            ((N >= 0), False),
            ((P >= 0), True),
            ((N <  0), True),
            ((P <  0), False),
            ((N <= 0), True),
            ((P <= 0), False),
            ((N >  28.71), False),
            ((P >  28.71), True),
            ((N >= 28.71), False),
            ((P >= 28.71), True),
            ((N <  28.71), True),
            ((P <  28.71), False),
            ((N <= 28.71), True),
            ((P <= 28.71), False),

            (N.__gt__("Dummy"), NotImplemented),
            (P.__gt__("Dummy"), NotImplemented),
            (N.__ge__("Dummy"), NotImplemented),
            (P.__ge__("Dummy"), NotImplemented),
            (N.__lt__("Dummy"), NotImplemented),
            (P.__lt__("Dummy"), NotImplemented),
            (N.__le__("Dummy"), NotImplemented),
            (P.__le__("Dummy"), NotImplemented),
        
            (N.__gt__(1.2+0.3j), NotImplemented),
            (P.__gt__(1.2+0.3j), NotImplemented),
            (N.__ge__(1.2+0.3j), NotImplemented),
            (P.__ge__(1.2+0.3j), NotImplemented),
            (N.__lt__(1.2+0.3j), NotImplemented),
            (P.__lt__(1.2+0.3j), NotImplemented),
            (N.__le__(1.2+0.3j), NotImplemented),
            (P.__le__(1.2+0.3j), NotImplemented),
        ])
        return test()

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
                (empty1.unions,       set()),
            
                (empty2.conditions,   []),
                (empty2.sureset,      set()),
                (empty2.unsureset,    set()),
                (empty2.parents,      set()),
                (empty2.allParents,   set()),
                (empty2.type,         math.Set.Type.Finite),
                (empty2.unions,       set()),
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
                (    desc1.unions,       set()),
                (len(desc2.conditions),  1),
                (    desc2.sureset,      set()),
                (    desc2.unsureset,    set()),
                (    desc2.parents,      set()),
                (    desc2.allParents,   set()),
                (    desc2.type,         math.Set.Type.Descriptive),
                (    desc2.unions,       set()),
                (len(desc3.conditions),  2),
                (    desc3.sureset,      set()),
                (    desc3.unsureset,    set()),
                (    desc3.parents,      {math.Sets.R}),
                (    desc3.allParents,   {math.Sets.R, math.Sets.C}),
                (    desc3.type,         math.Set.Type.Descriptive),
                (    desc3.unions,       set()),
                (len(desc4.conditions),  2),
                (    desc4.sureset,      set()),
                (    desc4.unsureset,    set()),
                (    desc4.parents,      {math.Sets.R}),
                (    desc4.allParents,   {math.Sets.R, math.Sets.C}),
                (    desc4.type,         math.Set.Type.Descriptive),
                (    desc4.unions,       set()),
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
                (    lst1.unions,         set()),
                (len(lst2.conditions),    0),
                (    lst2.sureset,        {1, 2, "3"}),
                (    lst2.unsureset,      set()),
                (    lst2.parents,        set()),
                (    lst2.allParents,     set()),
                (    lst2.type,           math.Set.Type.Finite),
                (    lst2.unions,         set()),
                (len(lst3.conditions),    0),
                (    lst3.sureset,        set()),
                (    lst3.unsureset,      set()),
                (    lst3.parents,        set()),
                (    lst3.allParents,     set()),
                (    lst3.type,           math.Set.Type.Finite),
                (    lst3.unions,         set()),
                (len(lst4.conditions),    0),
                (    lst4.sureset,        set()),
                (    lst4.unsureset,      {1, 2, "3"}),
                (    lst4.parents,        set()),
                (    lst4.allParents,     set()),
                (    lst4.type,           math.Set.Type.Finite),
                (    lst4.unions,         set()),
                (len(lst5.conditions),    0),
                (    lst5.sureset,        {1, 2, "3"}),
                (    lst5.unsureset,      set()),
                (    lst5.parents,        set()),
                (    lst5.allParents,     set()),
                (    lst5.type,           math.Set.Type.Finite),
                (    lst5.unions,         set()),
                (len(lst6.conditions),    0),
                (    lst6.sureset,        {1, 2}),
                (    lst6.unsureset,      set()),
                (    lst6.parents,        {math.Sets.R}),
                (    lst6.allParents,     {math.Sets.R, math.Sets.C}),
                (    lst6.type,           math.Set.Type.Finite),
                (    lst6.unions,         set()),
                (len(lst7.conditions),    0),
                (    lst7.sureset,        set()),
                (    lst7.unsureset,      {1, 2}),
                (    lst7.parents,        {math.Sets.R}),
                (    lst7.allParents,     {math.Sets.R, math.Sets.C}),
                (    lst7.type,           math.Set.Type.Finite),
                (    lst7.unions,         set()),
                (len(set1.conditions),    0),
                (    set1.sureset,        set()),
                (    set1.unsureset,      set()),
                (    set1.parents,        set()),
                (    set1.allParents,     set()),
                (    set1.type,           math.Set.Type.Finite),
                (    set1.unions,         set()),
                (len(set2.conditions),    0),
                (    set2.sureset,        {1, 2, "3"}),
                (    set2.unsureset,      set()),
                (    set2.parents,        set()),
                (    set2.allParents,     set()),
                (    set2.type,           math.Set.Type.Finite),
                (    set2.unions,         set()),
                (len(set3.conditions),    0),
                (    set3.sureset,        set()),
                (    set3.unsureset,      set()),
                (    set3.parents,        set()),
                (    set3.allParents,     set()),
                (    set3.type,           math.Set.Type.Finite),
                (    set3.unions,         set()),
                (len(set4.conditions),    0),
                (    set4.sureset,        set()),
                (    set4.unsureset,      {1, 2, "3"}),
                (    set4.parents,        set()),
                (    set4.allParents,     set()),
                (    set4.type,           math.Set.Type.Finite),
                (    set4.unions,         set()),
                (len(set5.conditions),    0),
                (    set5.sureset,        {1, 2, "3"}),
                (    set5.unsureset,      set()),
                (    set5.parents,        set()),
                (    set5.allParents,     set()),
                (    set5.type,           math.Set.Type.Finite),
                (    set5.unions,         set()),
                (len(set6.conditions),    0),
                (    set6.sureset,        {1, 2}),
                (    set6.unsureset,      set()),
                (    set6.parents,        {math.Sets.R}),
                (    set6.allParents,     {math.Sets.R, math.Sets.C}),
                (    set6.type,           math.Set.Type.Finite),
                (    set6.unions,         set()),
                (len(set7.conditions),    0),
                (    set7.sureset,        set()),
                (    set7.unsureset,      {1, 2}),
                (    set7.parents,        {math.Sets.R}),
                (    set7.allParents,     {math.Sets.R, math.Sets.C}),
                (    set7.type,           math.Set.Type.Finite),
                (    set7.unions,         set()),
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
                (    s1.unions,          set()),
                (len(s2.conditions),     0),
                (    s2.sureset,         set()),
                (    s2.unsureset,       {1, 2, 3}),
                (    s2.parents,         {finUnsure}),
                (    s2.allParents,      {finUnsure}),
                (    s2.type,            math.Set.Type.Finite),
                (    s2.unions,          set()),
                (len(s3.conditions),     0),
                (    s3.sureset,         set()),
                (    s3.unsureset,       set()),
                (    s3.parents,         {empty}),
                (    s3.allParents,      {empty}),
                (    s3.type,            math.Set.Type.Finite),
                (    s3.unions,          set()),
                (len(s4.conditions),     0),
                (    s4.sureset,         set()),
                (    s4.unsureset,       set()),
                (    s4.parents,         {finUnsure}),
                (    s4.allParents,      {finUnsure}),
                (    s4.type,            math.Set.Type.Finite),
                (    s4.unions,          set()),
                (len(s5.conditions),     2), # 1 from the default of s5 and 1 because desc is its parent
                (    s5.sureset,         set()),
                (    s5.unsureset,       set()),
                (    s5.parents,         {desc}),
                (    s5.allParents,      {desc}),
                (    s5.type,            math.Set.Type.Descriptive),
                (    s5.unions,          set()),
                (len(s6.conditions),     2), # 1 from the construction of s6 and 1 because desc is its parent
                (    s6.sureset,         set()),
                (    s6.unsureset,       set()),
                (    s6.parents,         {desc}),
                (    s6.allParents,      {desc}),
                (    s6.type,            math.Set.Type.Descriptive),
                (    s6.unions,          set()),
                (len(s7.conditions),     2), # 1 from the default of s7 and 1 because descCond is its parent
                (    s7.sureset,         set()),
                (    s7.unsureset,       set()),
                (    s7.parents,         {descCond}),
                (    s7.allParents,      {descCond}),
                (    s7.type,            math.Set.Type.Descriptive),
                (    s7.unions,          set()),
                (len(s8.conditions),     2), # 1 from the construction of s8 and 1 because descCond is its parent
                (    s8.sureset,         set()),
                (    s8.unsureset,       set()),
                (    s8.parents,         {descCond}),
                (    s8.allParents,      {descCond}),
                (    s8.type,            math.Set.Type.Descriptive),
                (    s8.unions,          set()),
                (len(s9.conditions),     3), # 2 from the construction of s9 and 1 because descCond is its parent
                (    s9.sureset,         set()),
                (    s9.unsureset,       set()),
                (    s9.parents,         {descCond}),
                (    s9.allParents,      {descCond}),
                (    s9.type,            math.Set.Type.Descriptive),
                (    s9.unions,          set()),
                (len(s10.conditions),    2), # 1 from the default construction of s10 and 1 because 'descCond | finSure' is listed as a parent
                (    s10.sureset,        {1, 2, 3}),
                (    s10.unsureset,      set()),
                (    s10.parents,        {union}),
                (    s10.allParents,     {union}),
                (    s10.type,           math.Set.Type.Union),
                (    s10.unions,         {descCond}),
                (len(s11.conditions),    3), # 1 from the construction of s11 and 2 because 'descCond | finSure' is listed as a parent and R is a parent
                (    s11.sureset,        set()),
                (    s11.unsureset,      {1, 2, 3}),
                (    s11.parents,        {union, math.Sets.R}),
                (    s11.allParents,     {union, math.Sets.R, math.Sets.C}),
                (    s11.type,           math.Set.Type.Union),
                (    s11.unions,         {descCond}),
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
    def IsSubSetOf() -> TestResult:
        """Tests if the isSubSetOf method works as intended"""
        empty1 = math.Set(math.Set.Empty)
        empty2 = math.Set(math.Set.Empty, [lambda x: False])
        fin1 = math.Set({1, 2, 3})
        des1 = math.Set(math.Set.Descriptive, parents={fin1})

        test = EqualityTester([
            (des1.isSubSetOf("dummy"),                              math.Result.FALSE),
            (des1.isSubSetOf(des1),                                 math.Result.TRUE),
            (des1.isSubSetOf(des1),                                 math.Result.TRUE),
            (des1.isSubSetOf(fin1),                                 math.Result.TRUE),
            (empty1.isSubSetOf(1),                                  math.Result.FALSE),
            (empty1.isSubSetOf(empty1),                             math.Result.TRUE),
            (empty1.isSubSetOf(empty2),                             math.Result.TRUE),
            (empty1.isSubSetOf(math.Set(math.Set.Descriptive)),     math.Result.TRUE),
            (empty1.isSubSetOf(math.Set({1,2})),                    math.Result.TRUE),
            (empty2.isSubSetOf(1),                                  math.Result.FALSE),
            (empty2.isSubSetOf(empty1),                             math.Result.TRUE),
            (empty2.isSubSetOf(empty2),                             math.Result.TRUE),
            (empty2.isSubSetOf(math.Set(math.Set.Descriptive)),     math.Result.TRUE),
            (empty2.isSubSetOf(math.Set({1,2})),                    math.Result.TRUE),
            # TODO: finish
        ])
        return test()
    def Contains() -> TestResult:
        """Tests if the contains method works as intended"""
        fin1 = math.Set({ 1, 2,"3"}, [lambda x: math.Result.TRUE if isinstance(x, int) else math.Result.UNSURE])
        des1 = math.Set(math.Set.Descriptive, [lambda x: isinstance(x, Real)], {fin1})
        des2 = math.Set(math.Set.Descriptive, [lambda x: isinstance(x, int)])
        des3 = math.Set(math.Set.Descriptive, [lambda x: math.Result.TRUE if isinstance(x, Real) else math.Result.UNSURE], {fin1})
        des4 = math.Set(math.Set.Descriptive, [lambda x: math.Result.TRUE if isinstance(x, Real) else math.Result.UNSURE])
        uni1 = fin1 | des1
        uni2 = des2 | des4

        test = EqualityTester([
            (fin1.contains(1),          math.Result.TRUE),
            (fin1.contains(2),          math.Result.TRUE),
            (fin1.contains("3"),        math.Result.UNSURE),
            (fin1.contains("dummy"),    math.Result.FALSE),
            (des1.contains(2),          math.Result.TRUE),
            (des1.contains(4),          math.Result.FALSE),
            (des2.contains(2),          math.Result.TRUE),
            (des2.contains(4),          math.Result.TRUE),
            (des3.contains(2),          math.Result.TRUE),
            (des3.contains("3"),        math.Result.UNSURE),
            (des3.contains(3),          math.Result.FALSE),
            (uni1.contains(3),          math.Result.FALSE),
            (uni1.contains(2),          math.Result.TRUE),
            (uni1.contains("3"),        math.Result.UNSURE),
            (uni2.contains(2),          math.Result.TRUE),
            (uni2.contains(2.5),        math.Result.TRUE),
            (uni2.contains("dummy"),    math.Result.UNSURE),
        ])
        
        res = test()

        badboi = math.Set(math.Set.Empty)
        badboi.type = 20
        try:
            badboi.contains("dummy")
            res += FAILED
        except(ValueError): res += PASSED

        return res
    def Union() -> TestResult:
        """Tests if the __or__ method works as intended"""
        s1 = math.Set(math.Set.Empty)
        fin1 = math.Set({ 1, 2, 3 })
        fin2 = math.Set({ 3, 4, 5 })
        fin3 = math.Set({ 1, 2,"3"}, [lambda x: math.Result.TRUE if isinstance(x, int) else math.Result.UNSURE])
        desc1 = math.Set(math.Set.Descriptive, [lambda x: math.Result.UNSURE])
        desc2 = math.Set(math.Set.Descriptive)

        uff1 = s1 | s1
        uff2 = s1 | fin1
        uff3 = fin1 | s1
        uff4 = fin1 | fin2
        uff5 = fin2 | fin1
        uff6 = fin1 | fin3
        uff7 = fin3 | fin1
        testFinFin = EqualityTester([
            (uff1.conditions, []),
            (uff1.sureset,    set()),
            (uff1.unsureset,  set()),
            (uff1.parents,    set()),
            (uff1.allParents, set()),
            (uff1.type,       math.Set.Type.Finite),
            (uff1.unions,     set()),
            (uff2.conditions, []),
            (uff2.sureset,    { 1, 2, 3 }),
            (uff2.unsureset,  set()),
            (uff2.parents,    set()),
            (uff2.allParents, set()),
            (uff2.type,       math.Set.Type.Finite),
            (uff2.unions,     set()),
            (uff3.conditions, []),
            (uff3.sureset,    { 1, 2, 3 }),
            (uff3.unsureset,  set()),
            (uff3.parents,    set()),
            (uff3.allParents, set()),
            (uff3.type,       math.Set.Type.Finite),
            (uff3.unions,     set()),
            (uff4.conditions, []),
            (uff4.sureset,    { 1, 2, 3, 4, 5 }),
            (uff4.unsureset,  set()),
            (uff4.parents,    set()),
            (uff4.allParents, set()),
            (uff4.type,       math.Set.Type.Finite),
            (uff4.unions,     set()),
            (uff5.conditions, []),
            (uff5.sureset,    { 1, 2, 3, 4, 5 }),
            (uff5.unsureset,  set()),
            (uff5.parents,    set()),
            (uff5.allParents, set()),
            (uff5.type,       math.Set.Type.Finite),
            (uff5.unions,     set()),
            (uff6.conditions, []),
            (uff6.sureset,    { 1, 2, 3 }),
            (uff6.unsureset,  { "3" }),
            (uff6.parents,    set()),
            (uff6.allParents, set()),
            (uff6.type,       math.Set.Type.Finite),
            (uff6.unions,     set()),
            (uff7.conditions, []),
            (uff7.sureset,    { 1, 2, 3 }),
            (uff7.unsureset,  { "3" }),
            (uff7.parents,    set()),
            (uff7.allParents, set()),
            (uff7.type,       math.Set.Type.Finite),
            (uff7.unions,     set()),
        ])
        
        ufd1 = fin3 | desc1
        ufd2 = desc1 | fin3
        testFinDesc = EqualityTester([
            (ufd1.conditions, []),
            (ufd1.sureset,    { 1, 2 }),
            (ufd1.unsureset,  { "3" }),
            (ufd1.parents,    set()),
            (ufd1.allParents, set()),
            (ufd1.type,       math.Set.Type.Union),
            (ufd1.unions,     {desc1}),
            (ufd2.conditions, []),
            (ufd2.sureset,    { 1, 2 }),
            (ufd2.unsureset,  { "3" }),
            (ufd2.parents,    set()),
            (ufd2.allParents, set()),
            (ufd2.type,       math.Set.Type.Union),
            (ufd2.unions,     {desc1}),
        ])

        udd1 = desc1 | desc2
        udd2 = desc2 | desc1
        testDescDesc = EqualityTester([
            (udd1.conditions, []),
            (udd1.sureset,    set()),
            (udd1.unsureset,  set()),
            (udd1.parents,    set()),
            (udd1.allParents, set()),
            (udd1.type,       math.Set.Type.Union),
            (udd1.unions,     {desc1, desc2}),
            (udd2.conditions, []),
            (udd2.sureset,    set()),
            (udd2.unsureset,  set()),
            (udd2.parents,    set()),
            (udd2.allParents, set()),
            (udd2.type,       math.Set.Type.Union),
            (udd2.unions,     {desc1, desc2}),
        ])

        ufu1 = fin3 | ufd1
        ufu2 = ufd1 | fin3
        ufu3 = fin2 | ufu1
        ufu4 = ufu1 | fin2
        testFinUnion = EqualityTester([
            (ufu1.conditions, []),
            (ufu1.sureset,    {1, 2}),
            (ufu1.unsureset,  {"3"}),
            (ufu1.parents,    set()),
            (ufu1.allParents, set()),
            (ufu1.type,       math.Set.Type.Union),
            (ufu1.unions,     {desc1}),
            (ufu2.conditions, []),
            (ufu2.sureset,    {1, 2}),
            (ufu2.unsureset,  {"3"}),
            (ufu2.parents,    set()),
            (ufu2.allParents, set()),
            (ufu2.type,       math.Set.Type.Union),
            (ufu2.unions,     {desc1}),
            (ufu3.conditions, []),
            (ufu3.sureset,    {1, 2, 3, 4, 5}),
            (ufu3.unsureset,  {"3"}),
            (ufu3.parents,    set()),
            (ufu3.allParents, set()),
            (ufu3.type,       math.Set.Type.Union),
            (ufu3.unions,     {desc1}),
            (ufu4.conditions, []),
            (ufu4.sureset,    {1, 2, 3, 4, 5}),
            (ufu4.unsureset,  {"3"}),
            (ufu4.parents,    set()),
            (ufu4.allParents, set()),
            (ufu4.type,       math.Set.Type.Union),
            (ufu4.unions,     {desc1}),
        ])

        udu1 = ufd1 | desc1
        udu2 = desc1 | ufd1
        udu3 = ufd1 | desc2
        udu4 = desc2 | ufd1
        testDescUnion = EqualityTester([
            (udu1.conditions, []),
            (udu1.sureset,    {1, 2}),
            (udu1.unsureset,  {"3"}),
            (udu1.parents,    set()),
            (udu1.allParents, set()),
            (udu1.type,       math.Set.Type.Union),
            (udu1.unions,     {desc1}),
            (udu2.conditions, []),
            (udu2.sureset,    {1, 2}),
            (udu2.unsureset,  {"3"}),
            (udu2.parents,    set()),
            (udu2.allParents, set()),
            (udu2.type,       math.Set.Type.Union),
            (udu2.unions,     {desc1}),
            (udu3.conditions, []),
            (udu3.sureset,    {1, 2}),
            (udu3.unsureset,  {"3"}),
            (udu3.parents,    set()),
            (udu3.allParents, set()),
            (udu3.type,       math.Set.Type.Union),
            (udu3.unions,     {desc1, desc2}),
            (udu4.conditions, []),
            (udu4.sureset,    {1, 2}),
            (udu4.unsureset,  {"3"}),
            (udu4.parents,    set()),
            (udu4.allParents, set()),
            (udu4.type,       math.Set.Type.Union),
            (udu4.unions,     {desc1, desc2}),
        ])

        uuu1 = ufd1 | udu1
        uuu2 = udu1 | ufd1
        uuu3 = (fin1 | desc1) | (fin3 | desc2)
        uuu4 = (fin3 | desc2) | (fin1 | desc1)
        testUnionUnion = EqualityTester([
            (uuu1.conditions, []),
            (uuu1.sureset,    {1, 2}),
            (uuu1.unsureset,  {"3"}),
            (uuu1.parents,    set()),
            (uuu1.allParents, set()),
            (uuu1.type,       math.Set.Type.Union),
            (uuu1.unions,     {desc1}),
            (uuu2.conditions, []),
            (uuu2.sureset,    {1, 2}),
            (uuu2.unsureset,  {"3"}),
            (uuu2.parents,    set()),
            (uuu2.allParents, set()),
            (uuu2.type,       math.Set.Type.Union),
            (uuu2.unions,     {desc1}),
            (uuu3.conditions, []),
            (uuu3.sureset,    {1, 2, 3}),
            (uuu3.unsureset,  {"3"}),
            (uuu3.parents,    set()),
            (uuu3.allParents, set()),
            (uuu3.type,       math.Set.Type.Union),
            (uuu3.unions,     {desc1, desc2}),
            (uuu4.conditions, []),
            (uuu4.sureset,    {1, 2, 3}),
            (uuu4.unsureset,  {"3"}),
            (uuu4.parents,    set()),
            (uuu4.allParents, set()),
            (uuu4.type,       math.Set.Type.Union),
            (uuu4.unions,     {desc1, desc2}),
        ])

        p1 = math.Set({1, 2, 3}, parents={math.Sets.R})
        p2 = math.Set({1, 2, 3}, parents={math.Sets.C})
        p3 = math.Set({1, 2, 3}, parents={p2})

        up1 = p1 | p2
        up2 = p2 | p1
        up3 = p3 | p1
        up4 = p1 | p3
        testParents = EqualityTester([
            (up1.conditions, []),
            (up1.sureset,    {1, 2, 3}),
            (up1.unsureset,  set()),
            (up1.parents,    {math.Sets.C}),
            (up1.allParents, {math.Sets.C}),
            (up1.type,       math.Set.Type.Finite),
            (up1.unions,     set()),
            (up2.conditions, []),
            (up2.sureset,    {1, 2, 3}),
            (up2.unsureset,  set()),
            (up2.parents,    {math.Sets.C}),
            (up2.allParents, {math.Sets.C}),
            (up2.type,       math.Set.Type.Finite),
            (up2.unions,     set()),
            (up3.conditions, []),
            (up3.sureset,    {1, 2, 3}),
            (up3.unsureset,  set()),
            (up3.parents,    {math.Sets.C}),
            (up3.allParents, {math.Sets.C}),
            (up3.type,       math.Set.Type.Finite),
            (up3.unions,     set()),
            (up4.conditions, []),
            (up4.sureset,    {1, 2, 3}),
            (up4.unsureset,  set()),
            (up4.parents,    {math.Sets.C}),
            (up4.allParents, {math.Sets.C}),
            (up4.type,       math.Set.Type.Finite),
            (up4.unions,     set()),
        ])

        res = testFinFin() + testFinDesc() + testDescDesc() + testFinUnion() + testDescUnion() + testUnionUnion() + testParents()

        badboi1 = math.Set({1, 2, 3})
        badboi1.type = 20
        try:
            badboi1|badboi1
            res += FAILED
        except(ValueError): res += PASSED
        try:
            fin1|badboi1
            res += FAILED
        except(ValueError): res += PASSED
        try:
            desc1|badboi1
            res += FAILED
        except(ValueError): res += PASSED
        try:
            uuu1|badboi1
            res += FAILED
        except(ValueError): res += PASSED

        try:
            fin1 | "dummy"
            res += FAILED
        except(TypeError): res += PASSED
        
        try:
            "dummy" | fin1
            res += FAILED
        except(TypeError): res += PASSED

        return res
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
