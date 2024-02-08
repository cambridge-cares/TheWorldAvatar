from dataclasses import dataclass
from typing import Dict, List, Optional


@dataclass
class OSProperty:
    value: float
    unit: str
    reference_state_value: Optional[float]
    reference_state_unit: Optional[str]


@dataclass
class OSSpecies:
    iri: str
    key2identifier: Dict[str, List[str]]
    key2property: Dict[str, List[OSProperty]]
    chemclasses: List[str]
    uses: List[str]
