from constants.namespace import ONTOCOMPCHEM, ONTOSPECIES
from model.ontospecies import GcAtom
from model.rdf_orm import RDFEntity, RDFField


class OntocompchemOptimizedGeometry(RDFEntity):
    atom: list[GcAtom] = RDFField(
        path=~ONTOSPECIES.fromGeometry / ~ONTOSPECIES.hasXCoordinate
    )


class OntocompchemRotationalConstants(RDFEntity):
    value: list[float] = RDFField(path=ONTOCOMPCHEM.value)
    unit: str = RDFField(path=ONTOCOMPCHEM.unit)


class OntocompchemHasValueHasUnit(RDFEntity):
    value: float = RDFField(path=ONTOCOMPCHEM.value)
    unit: str = RDFField(path=ONTOCOMPCHEM.unit)
