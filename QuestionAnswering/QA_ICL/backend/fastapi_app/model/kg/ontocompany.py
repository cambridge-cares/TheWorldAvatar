from rdflib import RDFS
from model.rdf_ogm import RDFEntity, RDFField


class OntocompanyIndustrialFacility(RDFEntity):
    label: str = RDFField(path=RDFS.label)