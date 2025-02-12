
from typing import Iterator, Self, Any

class SuccessorIterator(Iterator):
    """Iterator that only counts up and never stops. Can create infinite loops, so be careful"""
    def __init__(self):
        self.count = 0
    def __iter__(self) -> Self:
        return self
    def __next__(self) -> int:
        count = self.count
        self.count += 1
        return count

class JoinedIterator(Iterator):
    """Iterator that alternates between its children until they are depleted"""
    def __init__(self, children:list[Iterator]):
        if type(children) != list: raise TypeError("children parameter should be a list of Iterators")
        if set([isinstance(child, Iterator) for child in children]) != {True}: raise TypeError("children parameter should be a list of Iterators")
        self.children = children
        self.index = 0
    def __iter__(self) -> Self:
        return self
    def __next__(self) -> Any:
        while len(self.children) != 0:
            child = self.children[self.index % len(self.children)]
            self.index += 1
            try:
                return child.__next__()
            except(StopIteration):
                del self.children[(self.index-1) % len(self.children)]
        raise StopIteration