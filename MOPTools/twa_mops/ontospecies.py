from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty
import om


# NOTE TODO this script is incomplete
# NOTE TODO it should also be moved to a place that accessible to all other ontologies
class OntoSpecies(BaseOntology):
    base_url = 'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#'


# object properties
HasMolecularWeight = ObjectProperty.create_from_base('HasMolecularWeight', OntoSpecies)
HasCharge = ObjectProperty.create_from_base('HasCharge', OntoSpecies)


# data properties
HasGeometryFile = DatatypeProperty.create_from_base('HasGeometryFile', OntoSpecies)


# classes
class Charge(BaseClass):
    rdfs_isDefinedBy = OntoSpecies
    hasValue: om.HasValue[om.Measure]

class MolecularWeight(BaseClass):
    rdfs_isDefinedBy = OntoSpecies
    hasValue: om.HasValue[om.Measure]

class Geometry(BaseClass):
    rdfs_isDefinedBy = OntoSpecies
    hasGeometryFile: HasGeometryFile[str]
