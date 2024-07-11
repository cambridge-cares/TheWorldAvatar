from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty
from twa.data_model.base_ontology import as_range


class RandomOntology(BaseOntology):
    base_url = 'http://www.example.com/ontology/random.owl#'


class NumOfPoints(BaseClass):
    is_defined_by_ontology = RandomOntology
    hasValue: HasValue


class UpperLimit(BaseClass):
    is_defined_by_ontology = RandomOntology
    hasValue: HasValue


class LowerLimit(BaseClass):
    is_defined_by_ontology = RandomOntology
    hasValue: HasValue


class Point(BaseClass):
    is_defined_by_ontology = RandomOntology
    hasValue: HasValue
    specialValue: SpecialValue


class MaxValue(BaseClass):
    is_defined_by_ontology = RandomOntology
    hasValue: HasValue


class MinValue(BaseClass):
    is_defined_by_ontology = RandomOntology
    hasValue: HasValue


class Difference(BaseClass):
    is_defined_by_ontology = RandomOntology
    hasValue: HasValue


class DifferenceReverse(BaseClass):
    is_defined_by_ontology = RandomOntology
    hasValue: HasValue


class HasValue(DatatypeProperty):
    is_defined_by_ontology = RandomOntology
    range: as_range(int, 1, 1)


class SpecialValue(DatatypeProperty):
    is_defined_by_ontology = RandomOntology
    range: as_range(float)


if __name__ == '__main__':#
    print(NumOfPoints.model_fields.items())
    print(HasValue.get_predicate_iri())
    print(NumOfPoints.model_fields.items())
    print(NumOfPoints.model_rebuild())
    print(NumOfPoints.model_fields.items())
    print(NumOfPoints.model_rebuild())
    print(NumOfPoints.model_fields.items())
    
    