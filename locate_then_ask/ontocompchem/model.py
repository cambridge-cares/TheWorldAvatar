from dataclasses import dataclass
from typing import List


@dataclass
class OCCMolecularComputation:
    iri: str
    species_iri: str
    level_of_theory: str
    basis_set: str

@dataclass
class OCCSpecies:
    iri: str
    label: str
    molecular_computation_iris: List[str]