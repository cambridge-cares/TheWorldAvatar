from enum import Enum
from typing import Optional, Tuple, Union

from pydantic.dataclasses import dataclass


class ComparativeOperator(Enum):
    EQ = "="
    GT = ">"
    GE = ">="
    LT = "<"
    LE = "<="


@dataclass
class UnaryComparativeConstraint:
    operator: ComparativeOperator
    operand: float
    unit: Optional[str] = None

    def __str__(self):
        result = "{operator} {operand}".format(
            operator=self.operator.value, operand=self.operand
        )
        if self.unit:
            result += self.unit
        return result


class LogicalOperator(Enum):
    AND = "&&"
    OR = "||"


@dataclass
class CompoundComparativeConstraint:
    logical_operator: Optional[LogicalOperator] = None
    constraints: Tuple[UnaryComparativeConstraint, ...] = tuple()

    def __str__(self):
        if not self.constraints:
            return ""

        if self.logical_operator is LogicalOperator.OR:
            delimiter = " or "
        else:
            delimiter = " and "

        return delimiter.join([str(x) for x in self.constraints])


class ExtremeValueConstraint(Enum):
    MIN = "MIN"
    MAX = "MAX"

    def __str__(self):
        return self.value.lower()


NumericalConstraint = Union[CompoundComparativeConstraint, ExtremeValueConstraint]
