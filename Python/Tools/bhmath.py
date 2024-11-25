from Tools.log import log, Severity

from enum import IntEnum
from typing import Union, Iterable, Callable, Iterator, Any

class Sets(IntEnum):
	Empty=0
	All=1
     
class Set:
    class _Generator:
        class _Internal:
            def __init__(self, gen, conditions):
                self.iter = iter(gen)
                self.conditions = conditions
            def _meetsConditions(self, x):
                return set([cond(x) for cond in self.conditions])=={True}
            def __iter__(self):
                return self
            def __next__(self):
                while not self._meetsConditions(x:=self.iter.__next__()): pass
                return x

        def __init__(self, gen, conditions):
            self.gen = gen
            self.conditions = conditions
        def __call__(self):
            return self.__iter__()
        def __iter__(self):
            return self._Internal(iter(self.gen), self.conditions)

    class _NotIterable:
        def __init__(self, msg):
            self.msg = msg
        def __call__(self):
            return self
        def __iter__(self):
            return self
        def __next__(self)->None:
            log(self.msg, Severity.crash)
            raise StopIteration()

    def __init__(self, base:Union[Sets, set, list, Iterable], conditions:Iterable[Callable[[Any], bool]] = [lambda x:True])->None:
        """Creates a new Set object

        args:
            base (set, list or Iterable): When type is set, Set or list, the Set will contain the (finite) number of items in the base with the conditions applied. When type is Iterable, the set is assumed infite, but any item can be generated by the generator, either sequentially or by index. When type is Sets, the base will be the specified enum value.
            conditions (function): Specifies extra conditions on the base that must be met for an item to be in the new Set
        """

        if type(base) == Sets:
            if base == Sets.Empty:
                self._finite = {}
                self._conditions = []
                self._generator = self._finite.__iter__
            elif base == Sets.All:
                self._finite = Sets.All
                self._conditions = conditions
                self._generator = self._NotIterable("This Set is infinite and therefore provides no generator.")
            else:
                log("Unkown Sets enum!", Severity.crash)
        elif type(base) == set or type(base) == list:
            self._finite = set([x for x in base if set([cond(x) for cond in conditions])=={True}]) # filter out all elements of base that do not meet the conditions criteria
            self._conditions = [] # after all conditions are applied to the _finite internal set, they serve no more purpose
            self._generator = self._finite.__iter__
        elif isinstance(base, Iterable):
            self._finite = None
            self._conditions = conditions
            self._generator = self._Generator(base, conditions)
        else:
            log("Unsupported type for Set base!", Severity.crash)

    
    def __iter__(self)->Iterator:
        return self._generator()