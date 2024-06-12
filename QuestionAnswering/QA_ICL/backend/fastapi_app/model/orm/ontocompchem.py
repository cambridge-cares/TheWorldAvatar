from model.orm.base import HasIRI, HasUnit, HasValueHasUnit
from model.orm.ontospecies import OsAtom


class OccOptimizedGeometry(HasIRI):
    Atoms: list[OsAtom]


class OccRotationalConstants(HasIRI, HasUnit):
    Values: list[float]


class OccCalculationResultDefault(HasIRI, HasValueHasUnit):
    pass
