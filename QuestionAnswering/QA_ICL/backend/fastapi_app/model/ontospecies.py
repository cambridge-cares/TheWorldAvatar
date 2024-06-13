from rdflib.namespace import RDFS

from constants.namespace import ONTOSPECIES
from model.rdf_orm import RDFEntity, RDFField


class OntospeciesHasValueHasUnit(RDFEntity):
    value: str = RDFField(path=ONTOSPECIES.value)
    unit: str = RDFField(path=ONTOSPECIES.unit / RDFS.label)


class OntospeciesProperty(RDFEntity):
    value: str | float = RDFField(path=ONTOSPECIES.value)
    unit: str | None = RDFField(default=None, path=ONTOSPECIES.unit)
    reference_state: OntospeciesHasValueHasUnit | None = RDFField(
        default=None, path=ONTOSPECIES.hasReferenceState
    )
    provenance: str | None = RDFField(
        default=None, path=ONTOSPECIES.hasProvenance / RDFS.label
    )


class GcAtom(RDFEntity):
    x: float = RDFField(path=ONTOSPECIES.hasXCoordinate / ONTOSPECIES.value)
    y: float = RDFField(path=ONTOSPECIES.hasYCoordinate / ONTOSPECIES.value)
    z: float = RDFField(path=ONTOSPECIES.hasZCoordinate / ONTOSPECIES.value)


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
