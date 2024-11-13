from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty


class RandomOntology(BaseOntology):
    base_url = 'http://www.example.com/ontology/random.owl#'


class NumOfPoints(BaseClass):
    rdfs_isDefinedBy = RandomOntology
    hasValue: HasValue[int]


class UpperLimit(BaseClass):
    rdfs_isDefinedBy = RandomOntology
    hasValue: HasValue[int]


class LowerLimit(BaseClass):
    rdfs_isDefinedBy = RandomOntology
    hasValue: HasValue[int]


class Point(BaseClass):
    rdfs_isDefinedBy = RandomOntology
    hasValue: HasValue[int]
    specialValue: SpecialValue[float]


class MaxValue(BaseClass):
    rdfs_isDefinedBy = RandomOntology
    hasValue: HasValue[int]


class MinValue(BaseClass):
    rdfs_isDefinedBy = RandomOntology
    hasValue: HasValue[int]


class Difference(BaseClass):
    rdfs_isDefinedBy = RandomOntology
    hasValue: HasValue[int]


class DifferenceReverse(BaseClass):
    rdfs_isDefinedBy = RandomOntology
    hasValue: HasValue[int]


HasValue = DatatypeProperty.create_from_base('HasValue', RandomOntology, 1, 1)


SpecialValue = DatatypeProperty.create_from_base('SpecialValue', RandomOntology)


if __name__ == '__main__':#
    print(NumOfPoints.model_fields.items())
    print(HasValue.predicate_iri)
    print(NumOfPoints.model_fields.items())
    print(NumOfPoints.model_rebuild())
    print(NumOfPoints.model_fields.items())
    print(NumOfPoints.model_rebuild())
    print(NumOfPoints.model_fields.items())
