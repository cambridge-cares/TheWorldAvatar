from rdflib import RDFS
from constants.namespace import ONTOMOPS
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
    modularity: int = RDFField(path=ONTOMOPS.hasModularity)
    planarity: str = RDFField(path=ONTOMOPS.hasPlanarity)
