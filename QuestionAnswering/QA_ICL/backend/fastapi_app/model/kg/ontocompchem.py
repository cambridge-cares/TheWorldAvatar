from typing import Annotated, Literal

from pydantic import Field
from constants.namespace import ONTOCOMPCHEM
from model.kg.ontospecies import GcAtom
from model.rdf_ogm import RDFEntity, RDFField


class OntocompchemOptimizedGeometry(RDFEntity):
    type: Literal["Optimized geometry"] = "Optimized geometry"
    atom: list[GcAtom]


class OntocompchemRotationalConstants(RDFEntity):
    type: Literal["Rotational constants"] = "Rotational constants"
    value: list[float] = RDFField(path=ONTOCOMPCHEM.value)
    unit: str = RDFField(path=ONTOCOMPCHEM.unit)


class OntocompchemHasValueHasUnit(RDFEntity):
    value: float = RDFField(path=ONTOCOMPCHEM.value)
    unit: str = RDFField(path=ONTOCOMPCHEM.unit)


class OntocompchemFrequencies(OntocompchemHasValueHasUnit):
    type: Literal["Frequencies"] = "Frequencies"


class OntocompchemHOMOEnergy(OntocompchemHasValueHasUnit):
    type: Literal["HOMO energy"] = "HOMO energy"


class OntocompchemHOMOMinus1Energy(OntocompchemHasValueHasUnit):
    type: Literal["HOMO-1 energy"] = "HOMO-1 energy"


class OntocompchemHOMOMinus2Energy(OntocompchemHasValueHasUnit):
    type: Literal["HOMO-2 energy"] = "HOMO-2 energy"


class OntocompchemLUMOEnergy(OntocompchemHasValueHasUnit):
    type: Literal["LUMO energy"] = "LUMO energy"


class OntocompchemLUMOPlus1Energy(OntocompchemHasValueHasUnit):
    type: Literal["LUMO+1 energy"] = "LUMO+1 energy"


class OntocompchemLUMOPlus2Energy(OntocompchemHasValueHasUnit):
    type: Literal["LUMO+2 energy"] = "LUMO+2 energy"


class OntocompchemRotationalSymmetryNumber(OntocompchemHasValueHasUnit):
    type: Literal["Rotational symmetry number"] = "Rotational symmetry number"


class OntocompchemSCFEnergy(OntocompchemHasValueHasUnit):
    type: Literal["SCF energy"] = "SCF energy"


class OntocompchemTotalEnergy(OntocompchemHasValueHasUnit):
    type: Literal["Total energy"] = "Total energy"


class OntocompchemTotalEnthalpy(OntocompchemHasValueHasUnit):
    type: Literal["Total enthalpy"] = "Total enthalpy"


class OntocompchemTotalGibbsFreeEnergy(OntocompchemHasValueHasUnit):
    type: Literal["Total Gibbs free energy"] = "Total Gibbs free energy"


class OntocompchemZeroPointEnergy(OntocompchemHasValueHasUnit):
    type: Literal["Zero-point energy"] = "Zero-point energy"


OntocompchemCalculationResult = Annotated[
    OntocompchemOptimizedGeometry
    | OntocompchemRotationalConstants
    | OntocompchemFrequencies
    | OntocompchemHOMOEnergy
    | OntocompchemHOMOMinus1Energy
    | OntocompchemHOMOMinus2Energy
    | OntocompchemLUMOEnergy
    | OntocompchemLUMOPlus1Energy
    | OntocompchemLUMOPlus2Energy
    | OntocompchemRotationalSymmetryNumber
    | OntocompchemSCFEnergy
    | OntocompchemTotalEnergy
    | OntocompchemTotalEnthalpy
    | OntocompchemTotalGibbsFreeEnergy
    | OntocompchemZeroPointEnergy,
    Field(discriminator="type"),
]
