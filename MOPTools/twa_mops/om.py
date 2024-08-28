from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty


# NOTE TODO this script is incomplete, and it should be moved to a place that accessible to all other ontologies
class OM(BaseOntology):
    base_url = 'http://www.ontology-of-units-of-measure.org/resource/om-2/'


# object properties
HasValue = ObjectProperty.create_from_base('HasValue', OM)
HasUnit = ObjectProperty.create_from_base('HasUnit', OM)


# data properties
HasNumericalValue = DatatypeProperty.create_from_base('HasNumericalValue', OM)


# classes
class Unit(BaseClass):
    rdfs_isDefinedBy = OM

class Measure(BaseClass):
    rdfs_isDefinedBy = OM
    hasUnit: HasUnit[Unit]
    hasNumericalValue: HasNumericalValue[float]
