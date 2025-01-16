from dataclasses import dataclass
from typing import Tuple


@dataclass(frozen=True)
class OKMechanism:
    # okin:ReactionMechanism
    iri: str
    doi: str  # okin:hasProvenance/op:hasDOI
    species_iris: Tuple[str, ...]  # okin:hasGasPhase/^okin:belongsToPhase
    reaction_iris: Tuple[str, ...]  # okin:hasReaction

    def __post_init__(self):
        object.__setattr__(self, "species_iris", tuple(self.species_iris))
        object.__setattr__(self, "reaction_iris", tuple(self.reaction_iris))


@dataclass(frozen=True)
class OKGasPhaseReaction:
    # okin:GasPhaseReaction
    iri: str
    equations: Tuple[str, ...]
    reactant_iris: Tuple[str, ...]
    product_iris: Tuple[str, ...]
    mechanism_iris: Tuple[str, ...]  # ^okin:hasReaction
    # okin:hasKineticModel

    def __post_init__(self):
        object.__setattr__(self, "equations", tuple(self.equations))
        object.__setattr__(self, "reactant_iris", tuple(self.reactant_iris))
        object.__setattr__(self, "product_iris", tuple(self.product_iris))
        object.__setattr__(self, "mechanism_iris", tuple(self.mechanism_iris))


@dataclass(frozen=True)
class OKSpecies:
    # (okin:Product + okin:Reactant) subclassof os:Species
    iri: str
    label: str  # skos:altLabel
    mechanism_iris: Tuple[str, ...]  # okin:belongsToPhase/^okin:hasGasPhase
    # okin:hasThermoModel --> okin:definedIn --> okin:ReactionMechanism
    # okin:hasTransportModel --> okin:definedIn --> okin:ReactionMechanism

    def __post_init__(self):
        object.__setattr__(self, "mechanism_iris", tuple(self.mechanism_iris))
