from dataclasses import dataclass, field
from typing import Tuple


@dataclass
class OKMechanism:
    # okin:ReactionMechanism
    iri: str
    doi: str  # okin:hasProvenance/op:hasDOI
    species_iris: Tuple[str, ...] = field(default_factory=tuple)  # okin:hasGasPhase/^okin:belongsToPhase
    reaction_iris: Tuple[str, ...] = field(default_factory=tuple)  # okin:hasReaction


@dataclass
class OKGasPhaseReaction:
    # okin:GasPhaseReaction
    iri: str
    equations: Tuple[str, ...] = field(default_factory=tuple)
    reactant_iris: Tuple[str, ...] = field(default_factory=tuple)
    product_iris: Tuple[str, ...] = field(default_factory=tuple)
    mechanism_iris: Tuple[str, ...] = field(default_factory=tuple)  # ^okin:hasReaction
    # okin:hasKineticModel


@dataclass
class OKSpecies:
    # (okin:Product + okin:Reactant) subclassof os:Species
    iri: str
    label: str  # skos:altLabel
    mechanism_iris: Tuple[str, ...] = field(default_factory=tuple)  # okin:belongsToPhase/^okin:hasGasPhase
    # okin:hasThermoModel --> okin:definedIn --> okin:ReactionMechanism
    # okin:hasTransportModel --> okin:definedIn --> okin:ReactionMechanism
