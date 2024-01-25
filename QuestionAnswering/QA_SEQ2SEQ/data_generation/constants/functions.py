from enum import Enum
import random
from typing import Tuple


class NumOp(Enum):
    LESS_THAN = "<"
    GREATER_THAN = ">"
    LESS_THAN_EQUAL = "<="
    GREATER_THAN_EQUAL = ">="
    EQUAL = "="
    AROUND = "around"
    INSIDE_RANGE = "in"
    OUTSIDE_RANGE = "outside"

class StrOp(Enum):
    VALUES = "values"

def lt_maker(x: float):
    return (
        random.choice(["<", "less than", "lower than", "smaller than"]) + " " + str(x)
    )


def gt_maker(x: float):
    return (
        random.choice([">", "greater than", "higher than", "bigger than"])
        + " "
        + str(x)
    )


def le_maker(x: float):
    return (
        random.choice(["<=", "less than or equal to", "not greater than"])
        + " "
        + str(x)
    )


def ge_maker(x: float):
    return (
        random.choice([">=", "greater than or equal to", "not less than"])
        + " "
        + str(x)
    )


def eq_maker(x: float):
    return random.choice(["=", "equal to"]) + " " + str(x)


def around_maker(x: float):
    return random.choice(["around", "approximately"]) + " " + str(x)


def inside_maker(x: Tuple[float, float]):
    if random.getrandbits(1):
        return "inside the range" + " " + str(x)
    else:
        return gt_maker(x[0]) + " and " + lt_maker(x[1])


def outside_maker(x: Tuple[float, float]):
    if random.getrandbits(1):
        return "outside the range" + " " + str(x)
    else:
        return lt_maker(x[0]) + " or " + gt_maker(x[1])


COMPARATIVE_COND_MAKER = {
    NumOp.LESS_THAN: lt_maker,
    NumOp.GREATER_THAN: gt_maker,
    NumOp.LESS_THAN_EQUAL: le_maker,
    NumOp.GREATER_THAN_EQUAL: ge_maker,
    NumOp.EQUAL: eq_maker,
    NumOp.AROUND: around_maker,
    NumOp.INSIDE_RANGE: inside_maker,
    NumOp.OUTSIDE_RANGE: outside_maker,
}
