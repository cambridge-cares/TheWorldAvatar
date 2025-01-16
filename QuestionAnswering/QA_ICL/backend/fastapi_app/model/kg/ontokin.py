from typing import Annotated, Literal
from pydantic import Field
from rdflib import RDFS

from constants.namespace import ONTOKIN, ONTOPROVENANCE
from model.rdf_ogm import RDFEntity, RDFField


class OntokinMechanismBase(RDFEntity):
    provenance: str = RDFField(
        path=ONTOKIN.hasProvenance / (ONTOPROVENANCE.hasDOI | ONTOPROVENANCE.hasURL)
    )


class OntokinReactionBase(RDFEntity):
    equation: str = RDFField(path=ONTOKIN.hasEquation)


class OntokinHasValueHasUnit(RDFEntity):
    value: float = RDFField(path=ONTOKIN.value)
    unit: str = RDFField(path=ONTOKIN.unit)


class OntokinPolynomial(RDFEntity):
    Tmin: OntokinHasValueHasUnit = RDFField(path=ONTOKIN.hasTmin)
    Tmax: OntokinHasValueHasUnit = RDFField(path=ONTOKIN.hasTmax)
    A1: float = RDFField(path=ONTOKIN.hasA1)
    A2: float = RDFField(path=ONTOKIN.hasA2)
    A3: float = RDFField(path=ONTOKIN.hasA3)
    A4: float = RDFField(path=ONTOKIN.hasA4)
    A5: float = RDFField(path=ONTOKIN.hasA5)
    A6: float = RDFField(path=ONTOKIN.hasA6)
    A7: float = RDFField(path=ONTOKIN.hasA7)
    B1: float = RDFField(path=ONTOKIN.hasB1)
    B2: float = RDFField(path=ONTOKIN.hasB2)


class OntokinThermoModel(RDFEntity):
    Tmin: OntokinHasValueHasUnit = RDFField(path=ONTOKIN.hasTmin)
    Tmax: OntokinHasValueHasUnit = RDFField(path=ONTOKIN.hasTmax)
    polynomials: list[OntokinPolynomial] = RDFField(path=ONTOKIN.hasPolynomial)


class OntokinTransportModel(RDFEntity):
    dipole_moment: OntokinHasValueHasUnit = RDFField(path=ONTOKIN.hasDipoleMoment)
    LJ_collision_diameter: OntokinHasValueHasUnit | None = RDFField(
        path=ONTOKIN.hasLJCollisionDiameter
    )
    LJ_potential_well_depth: OntokinHasValueHasUnit = RDFField(
        path=ONTOKIN.hasLJPotentialWellDepth
    )
    polarizability: OntokinHasValueHasUnit = RDFField(path=ONTOKIN.hasPolarizability)
    rotational_relaxation_collision_number: float = RDFField(
        path=ONTOKIN.hasRotationalRelaxationCollisionNumber / ONTOKIN.value
    )
    shape_index: int = RDFField(path=ONTOKIN.hasShapeIndex / ONTOKIN.value)


class OntokinArrheniusModelBase(RDFEntity):
    activation_energy: OntokinHasValueHasUnit = RDFField(
        path=ONTOKIN.hasActivationEnergy
    )
    Arrhenius_factor: OntokinHasValueHasUnit = RDFField(path=ONTOKIN.hasArrheniusFactor)
    temperature_exponent: float = RDFField(
        path=ONTOKIN.hasTemperatureExponent / ONTOKIN.value
    )


class OntokinArrheniusModel(OntokinArrheniusModelBase):
    type: Literal["Arrhenius model"] = "Arrhenius model"


class OntokinPDepArrheniusModel(OntokinArrheniusModelBase):
    type: Literal["Pressure-dependent Arrhenius model"] = (
        "Pressure-dependent Arrhenius model"
    )
    pressure: float = RDFField(path=(~ONTOKIN.isPressureConditionOf) / ONTOKIN.value)


class OntokinMultiArrheniusModel(RDFEntity):
    type: Literal["Multi-Arrhenius model"] = "Multi-Arrhenius model"
    Arrhenius_model: list[OntokinArrheniusModelBase] = RDFField(
        path=ONTOKIN.hasArrheniusModel
    )


class OntokinCollider(RDFEntity):
    label: str = RDFField(path=RDFS.label)
    efficiency: float = RDFField(path=ONTOKIN.hasEfficiency)


class OntokinThreeBodyReactionModel(RDFEntity):
    type: Literal["Three body reaction model"] = "Three body reaction model"
    collider: list[OntokinCollider] = RDFField(path=ONTOKIN.hasCollider)
    Arrhenius_low_model: OntokinArrheniusModelBase = RDFField(
        path=ONTOKIN.hasArrheniusLowModel
    )


class OntokinLindemannModel(OntokinThreeBodyReactionModel):
    type: Literal["Lindemann model"] = "Lindemann model"
    Arrhenius_high_model: OntokinArrheniusModelBase = RDFField(
        path=ONTOKIN.hasArrheniusHighModel
    )


class OntokinTroeModel(OntokinLindemannModel):
    type: Literal["Troe model"] = "Troe model"
    alpha: float = RDFField(path=ONTOKIN.hasAlpha / ONTOKIN.value)
    T1: float = RDFField(path=ONTOKIN.hasT1 / ONTOKIN.value)
    T2: float = RDFField(path=ONTOKIN.hasT2 / ONTOKIN.value)
    T3: float = RDFField(path=ONTOKIN.hasT3 / ONTOKIN.value)


OntokinKineticModelBase = Annotated[
    OntokinArrheniusModel
    | OntokinPDepArrheniusModel
    | OntokinMultiArrheniusModel
    | OntokinThreeBodyReactionModel
    | OntokinLindemannModel
    | OntokinTroeModel,
    Field(discriminator="type"),
]
