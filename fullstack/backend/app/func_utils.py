import logging
from typing import Callable, List, Type, TypeVar

T = TypeVar("T")  
J = TypeVar("J")  

def make_batches(data: List[T], batch_size: int = 10) -> List[List[T]]:
    return [data[i:i + batch_size] for i in range(0, len(data), batch_size)]



logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def pipe(value: T, *funcs: Callable[[T], T], final: Callable[[T], J]) -> J:
    for func in funcs:
        print(func)
        value = func(value)
    return final(value)


