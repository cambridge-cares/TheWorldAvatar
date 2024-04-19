from dataclasses import dataclass
from typing import List


@dataclass
class OCCMolecularComputation:
    iri: str
    species_iri: str # occ:hasSpeciesModel/occ:hasSpecies
    level_of_theory: str # occ:hasMethodology/occ:hasLevelOfTheory/rdfs:label
    basis_set: str

@dataclass
class OCCSpecies:
    iri: str
    label: str
    molecular_computation_iris: List[str]