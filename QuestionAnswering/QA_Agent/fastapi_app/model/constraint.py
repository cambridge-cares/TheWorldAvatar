from dataclasses import dataclass
from enum import Enum
from typing import Optional, Tuple


class NumericalOperator(Enum):
    EQ = "="
    GT = ">"
    GE = ">="
    LT = "<"
    LE = "<="

@dataclass
class AtomicNumericalConstraint:
    operator: NumericalOperator
    operand: float

class LogicalOperator(Enum):
    AND = "&&"
    OR = "||"

@dataclass
class CompoundNumericalConstraint:
    logical_operator: Optional[LogicalOperator]
    constraints: Tuple[AtomicNumericalConstraint]