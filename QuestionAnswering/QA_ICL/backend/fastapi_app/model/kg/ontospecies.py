from rdflib.namespace import RDFS

from constants.namespace import GC, ONTOSPECIES
from model.rdf_orm import RDFEntity, RDFField


class PeriodictableElement(RDFEntity):
    symbol: str = RDFField(path=ONTOSPECIES.hasElementSymbol / ONTOSPECIES.value)
    name: str = RDFField(path=ONTOSPECIES.hasElementName / ONTOSPECIES.value)


class OntospeciesHasValueHasUnit(RDFEntity):
    value: str = RDFField(path=ONTOSPECIES.value)
    unit: str | None = RDFField(default=None, path=ONTOSPECIES.unit / RDFS.label)


class OntospeciesProperty(OntospeciesHasValueHasUnit):
    reference_state: OntospeciesHasValueHasUnit | None = RDFField(
        default=None, path=ONTOSPECIES.hasReferenceState
    )
    provenance: str | None = RDFField(
        default=None, path=ONTOSPECIES.hasProvenance / RDFS.label
    )


class GcAtom(RDFEntity):
    element: PeriodictableElement = RDFField(path=GC.isElement)
    x: OntospeciesHasValueHasUnit = RDFField(path=ONTOSPECIES.hasXCoordinate)
    y: OntospeciesHasValueHasUnit = RDFField(path=ONTOSPECIES.hasYCoordinate)
    z: OntospeciesHasValueHasUnit = RDFField(path=ONTOSPECIES.hasZCoordinate)


class OntospeciesIdentifier(RDFEntity):
    value: str = RDFField(path=ONTOSPECIES.value)


class OntospeciesHasLabel(RDFEntity):
    label: str = RDFField(path=RDFS.label)


class OntospeciesSpecies(RDFEntity):
    label: str = RDFField(path=RDFS.label)
    IUPAC_name: OntospeciesIdentifier | None = RDFField(
        default=None, path=ONTOSPECIES.hasIUPACName
    )
    InChI: OntospeciesIdentifier = RDFField(path=ONTOSPECIES.hasInChI)
