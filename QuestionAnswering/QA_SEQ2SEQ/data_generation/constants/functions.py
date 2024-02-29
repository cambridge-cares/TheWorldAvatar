from enum import Enum


class NumOp(Enum):
    LESS_THAN = "<"
    GREATER_THAN = ">"
    LESS_THAN_EQUAL = "<="
    GREATER_THAN_EQUAL = ">="
    EQUAL = "="
    AROUND = "around"
    INSIDE_RANGE = "in"
    OUTSIDE_RANGE = "outside"


OBE_NUM_OPS = [
    NumOp.LESS_THAN,
    NumOp.GREATER_THAN,
    NumOp.LESS_THAN_EQUAL,
    NumOp.GREATER_THAN_EQUAL,
    NumOp.EQUAL,
    NumOp.INSIDE_RANGE,
]


class StrOp(Enum):
    VALUES = "values"


class AggOp(Enum):
    COUNT = "count"
    MIN = "min"
    MAX = "max"
    AVG = "avg"


EXTREMUM_OPS = [AggOp.MIN, AggOp.MAX]


AGG_OP_LABELS = {
    AggOp.MIN: ["minimum", "lowest"],
    AggOp.MAX: ["maximum", "highest"],
    AggOp.AVG: ["average"],
}


EXTREME_FREQ_LABELS = {
    AggOp.MIN: ["least common", "least popular", "rarest"],
    AggOp.MAX: ["most common", "most popular", "dominant"],
}
