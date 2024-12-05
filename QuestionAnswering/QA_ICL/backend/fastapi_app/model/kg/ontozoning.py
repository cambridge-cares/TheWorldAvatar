from rdflib import RDFS
from model.rdf_ogm import RDFEntity, RDFField


class OntozoningLandUseType(RDFEntity):
    label: str = RDFField(path=RDFS.label)
