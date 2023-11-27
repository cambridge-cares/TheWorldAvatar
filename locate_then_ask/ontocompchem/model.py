from dataclasses import dataclass


@dataclass
class MolecularComputation:
    species: str
    level_of_theory: str
    basis_set: str
