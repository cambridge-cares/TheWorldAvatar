from dataclasses import dataclass
from typing import Dict, List, Union


@dataclass
class OKThermoModel:
    # ontokin:NASA
    iri: str
    coeff_values: List[float]
    coeff_num: int
    max_temp: float
    min_temp: float
    pressure: float


@dataclass
class OKTransportModel:
    # ontokin:TransportModel
    iri: str
    dipole_momemnt: float
    dipole_moment_units: str
    LJ_diameter: float
    LJ_diameter_units: str
    LJ_well_depth: float
    LJ_well_depth_units: str
    polarizability: int
    polarizability_unit: str
    rotational_relaxation_collision_num: int
    rotational_relaxation_collision_num: str
    species_geometry: str
    species_geometry_title: str


@dataclass
class Mechanism:
    iri: str
    label: str


@dataclass
class OKSpecies:
    iri: str
    label: str
    thermo_model: OKThermoModel
    transport_model: OKTransportModel
    mechanism: Mechanism # ontokin:belongsToPhase/ontokin:containedIn


@dataclass
class ArrheniusCoefficient:
    # ontokin:ArrheniusCoefficient
    activation_energy: float
    activation_energy_units: str
    preexponential_factor: float
    preexponential_factor_units: str
    temp_exponent: float
    temp_exponent_units: str


@dataclass
class Reaction:
    # http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction
    iri: str
    equation: str
    arrhenius_coeff: ArrheniusCoefficient
    reactants: Union[str, OKSpecies]
    products: Union[str, OKSpecies]
    mechanism: Mechanism # ontokin:belongsToPhase/ontokin:containedIn
