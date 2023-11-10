from dataclasses import dataclass
from typing import List


@dataclass
class OKMechanism:
    # okin:ReactionMechanism
    iri: str
    doi: str  # okin:hasProvenance/oprvn:hasDOI
    species_iris: List[str]  # okin:hasGasPhase/^okin:belongsToPhase
    reaction_iris: List[str]  # okin:hasReaction


@dataclass
class OKGasePhaseReaction:
    # okin:GasPhaseReaction
    iri: str
    equations: List[str]
    reactant_iris: List[str]
    product_iris: List[str]
    mechanism_iris: List[str]  # okin:belongsToPhase/okin:containedIn
    # okin:hasKineticModel


@dataclass
class OKSpecies:
    # (okin:Product + okin:Reactant) subclassof os:Species
    iri: str
    label: str  # skos:altLabel
    mechanism_iris: List[str]  # okin:belongsToPhase/^okin:hasGasPhase
    # okin:hasThermoModel --> okin:definedIn --> okin:ReactionMechanism
    # okin:hasTransportModel --> okin:definedIn --> okin:ReactionMechanism
