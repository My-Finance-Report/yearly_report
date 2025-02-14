import logging
from typing import Callable, List, Type, TypeVar

def make_batches(data: List[str], batch_size: int = 10) -> List[List[str]]:
    return [data[i:i + batch_size] for i in range(0, len(data), batch_size)]


T = TypeVar("T")  
J = TypeVar("J")  

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)



def safe_pipe(value: T, *funcs: Callable[[T], T], final: Callable[[T], J]) -> J:
    for func in funcs:
        try:
            value = func(value)
        except Exception as e:
            logger.error(f"Error in {func.__name__}: {e}")
            break  
    return final(value)


