from enum import Enum


class AggregateOperator(Enum):
    MIN = "MIN"
    MAX = "MAX"
    AVG = "AVG"
    COUNT = "COUNT"
    SUM = "SUM"