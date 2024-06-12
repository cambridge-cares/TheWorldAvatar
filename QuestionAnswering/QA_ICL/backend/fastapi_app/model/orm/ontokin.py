from typing import Annotated, Literal
from pydantic import Field

from model.orm.base import HasIRI, HasLabel, HasProvenance, HasValueHasUnit


class OkinMechanism(HasIRI, HasProvenance):
    pass


class OkinReaction(HasIRI):
    Equation: str


class OkinArrheniusModelBase:
    ActivationEnergy: HasValueHasUnit
    ArrheniusFactor: HasValueHasUnit
    TemperatureExponent: float


class OkinArrheniusModel(HasIRI, OkinArrheniusModelBase):
    Type: Literal["Arrhenius model"] = "Arrhenius model"


class OkinPDepArrheniusModel(HasIRI, OkinArrheniusModelBase):
    Type: Literal["Pressure-dependent Arrhenius model"] = (
        "Pressure-dependent Arrhenius model"
    )
    Pressure: float


class OkinMultiArrheniusModel(HasIRI):
    Type: Literal["Multi-Arrhenius model"] = "Multi-Arrhenius model"
    ArrheniusModels: list[OkinArrheniusModelBase]


class OkinCollider(HasLabel):
    Efficiency: float


class OkinThreeBodyReactionModel(HasIRI):
    Type: Literal["Three-body reaction model"] = "Three-body reaction model"
    Colliders: list[OkinCollider]
    ArrheniusLowModel: OkinArrheniusModelBase


class OkinLindemannModel(OkinThreeBodyReactionModel):
    Type: Literal["Lindemann model"] = "Lindemann model"
    ArrheniusHighModel: OkinArrheniusModelBase


class OkinTroeModel(OkinLindemannModel):
    Type: Literal["Troe model"] = "Troe model"
    Alpha: float
    T1: float
    T2: float
    T3: float


OkinKineticModel = Annotated[
    OkinArrheniusModel
    | OkinPDepArrheniusModel
    | OkinMultiArrheniusModel
    | OkinThreeBodyReactionModel
    | OkinLindemannModel
    | OkinTroeModel,
    Field(discriminator="Type"),
]


class OkinPolynomial:
    Tmin: HasValueHasUnit
    Tmax: HasValueHasUnit
    A1: float
    A2: float
    A3: float
    A4: float
    A5: float
    A6: float
    A7: float
    B1: float
    B2: float


class OkinThermoModel(HasIRI):
    Tmin: HasValueHasUnit
    Tmax: HasValueHasUnit
    Polynomials: list[OkinPolynomial]


class OkinTransportModel(HasIRI):
    DipoleMoment: HasValueHasUnit
    LJCollisionDiamater: HasValueHasUnit
    LJPotentialWellDepth: HasValueHasUnit
    Polarizability: HasValueHasUnit
    RotationalRelaxationCollisionNumber: HasValueHasUnit
    ShapeIndex: int
