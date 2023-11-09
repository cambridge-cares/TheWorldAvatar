from dataclasses import dataclass
from typing import List, Optional


@dataclass
class OKThermoModel:
    # okin:NASA
    coeff_values: str
    coeff_num: int
    max_temp: float
    min_temp: float
    pressure: float


@dataclass
class OKTransportModel:
    # okin:TransportModel
    dipole_momemnt: float
    dipole_moment_units: str
    LJ_diameter: float
    LJ_diameter_units: str
    LJ_well_depth: float
    LJ_well_depth_units: str
    polarizability: float
    polarizability_units: str
    rotational_relaxation_collision_num: float
    rotational_relaxation_collision_num_units: str
    species_geometry: str
    species_geometry_title: str


@dataclass
class OKMechanism:
    iri: str
    label: str
    species_iris: List[str]
    reaction_iris: List[str]


@dataclass
class OKSpecies:
    # okin:Species
    iri: str
    label: str
    thermo_models: List[OKThermoModel]
    transport_model: Optional[OKTransportModel]
    mechanism: OKMechanism # okin:belongsToPhase/okin:containedIn


@dataclass
class OKArrheniusCoefficient:
    # okin:ArrheniusCoefficient
    activation_energy: float
    activation_energy_units: str
    preexponential_factor: float
    preexponential_factor_units: str
    temp_exponent: float
    temp_exponent_units: str

@dataclass
class OKFallOffModelCoefficient:
    # okin:FallOoffModelCoefficient
    coeff_values: str
    coeff_num: int

@dataclass
class OCAPEReactant:
    iri: str
    label: str

@dataclass
class OCAPEProduct:
    iri: str
    label: str

@dataclass
class OKGasePhaseReaction:
    # In ABox, most reactions are of the type http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#ChemicalReaction
    iri: str
    equation: str
    arrhenius_coeffs: List[OKArrheniusCoefficient]
    falloff_coeff: Optional[OKFallOffModelCoefficient]
    # ignore other coeffs for now
    reactants: List[OCAPEReactant]
    products: List[OCAPEProduct]
    mechanism: OKMechanism # okin:belongsToPhase/okin:containedIn
