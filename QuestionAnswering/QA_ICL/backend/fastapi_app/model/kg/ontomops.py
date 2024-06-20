from constants.namespace import ONTOMOPS, ONTOSPECIES
from model.rdf_orm import RDFEntity, RDFField


class OntomopsMOP(RDFEntity):
    formula: str = RDFField(path=ONTOMOPS.hasMOPFormula)


class OntomopsCBU(RDFEntity):
    formula: str = RDFField(path=ONTOMOPS.hasCBUFormula)


class OntomopsAM(RDFEntity):
    value: str = RDFField(path=ONTOSPECIES.value)
    symmetry_point_group: str = RDFField(path=ONTOMOPS.hasSymmetryPointGroup)


class OntomopsGBU(RDFEntity):
    modularity: int = RDFField(path=ONTOMOPS.hasModularity)
    planarity: str = RDFField(path=ONTOMOPS.hasPlanarity)
