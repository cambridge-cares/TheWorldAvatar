from dataclasses import dataclass
from decimal import Decimal
from typing import Dict, List, Optional

from constants.ontospecies import OSIdentifierKey, OSPropertyKey


@dataclass(frozen=True)
class OSProperty:
    value: Decimal
    unit: str
    reference_state_value: Optional[float] = None
    reference_state_unit: Optional[str] = None


@dataclass(frozen=True)
class OSSpecies:
    iri: str
    key2identifier: Dict[OSIdentifierKey, List[str]]
    key2property: Dict[OSPropertyKey, List[OSProperty]]
    chemclasses: List[str]
    uses: List[str]
