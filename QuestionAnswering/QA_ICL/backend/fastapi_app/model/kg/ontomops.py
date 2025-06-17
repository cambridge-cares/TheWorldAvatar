from rdflib import RDFS
from constants.namespace import OM2, ONTOMOPS
from model.rdf_ogm import RDFEntity, RDFField


class OntomopsMOP(RDFEntity):
    formula: str = RDFField(path=ONTOMOPS.hasMOPFormula)
    provenance: str = RDFField(path=ONTOMOPS.hasProvenance / ONTOMOPS.hasReferenceDOI)


class OntomopsCBU(RDFEntity):
    formula: str = RDFField(path=ONTOMOPS.hasCBUFormula)


class OntomopsAM(RDFEntity):
    name: str | None = RDFField(path=RDFS.label)
    polyhedral_shape: str = RDFField(path=ONTOMOPS.hasPolyhedralShape)
    symmetry_point_group: str = RDFField(path=ONTOMOPS.hasSymmetryPointGroup)


class OntomopsGBU(RDFEntity):
    modularity: int = RDFField(path=ONTOMOPS.hasGBUType / ONTOMOPS.hasModularity)
    planarity: str = RDFField(path=ONTOMOPS.hasGBUType / ONTOMOPS.hasPlanarity)

class OntomopsGBUType(RDFEntity):
    modularity: int = RDFField(path=ONTOMOPS.hasModularity)
    planarity: str = RDFField(path=ONTOMOPS.hasPlanarity)
    
class OmMeasure(RDFEntity):
    unit: str = RDFField(path=OM2.hasUnit)
    numerical_value: float = RDFField(path=OM2.hasNumericalValue)