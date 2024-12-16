
from dataclasses import dataclass
from decimal import Decimal


@dataclass(frozen=True)
class OmMeasure:
    numerical_value: Decimal  # om:hasNumericalValue
    unit_iri: str  # om:hasUnit
