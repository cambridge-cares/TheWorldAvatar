from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty


# NOTE TODO this script is incomplete as it only contains necessary classes and properties for the MOPs project
# NOTE TODO a complete OGM representation for OntoSpecies is yet to be implemented
# NOTE TODO it should be moved to a place that accessible to all other ontologies
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

class SingularUnit(Unit):
    rdfs_isDefinedBy = OM

class CompoundUnit(Unit):
    rdfs_isDefinedBy = OM

class UnitDivision(CompoundUnit):
    rdfs_isDefinedBy = OM

class Quantity(BaseClass):
    rdfs_isDefinedBy = OM
    hasValue: HasValue[Measure]

    @property
    def num_value(self):
        return list(self.hasValue)[0].hasNumericalValue

    @property
    def unit(self):
        return list(self.hasValue)[0].hasUnit

class Length(Quantity):
    rdfs_isDefinedBy = OM

class Radius(Length):
    rdfs_isDefinedBy = OM

class Diameter(Length):
    rdfs_isDefinedBy = OM

class Measure(BaseClass):
    rdfs_isDefinedBy = OM
    hasUnit: HasUnit[Unit]
    hasNumericalValue: HasNumericalValue[float]

# TODO need to find a better way for defining the units
gramPerMole = OM.namespace_iri + 'gramPerMole'
elementaryCharge = OM.namespace_iri + 'elementaryCharge'
angstrom = OM.namespace_iri + 'angstrom'
