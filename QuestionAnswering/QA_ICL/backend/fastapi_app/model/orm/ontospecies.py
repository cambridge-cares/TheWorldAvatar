from model.orm.base import (
    HasIRI,
    HasLabel,
    HasProvenance,
    HasUnitOptional,
    HasValue,
    HasValueHasUnit,
)


class OsSpecies(HasIRI, HasLabel):
    IUPACName: str | None
    InChI: str


class OsProperty(HasIRI, HasValue, HasUnitOptional, HasProvenance):
    RefState: HasValueHasUnit | None


class OsIdentifier(HasIRI, HasValue):
    pass


class OsChemicalClass(HasIRI, HasLabel):
    pass


class OsUse(HasIRI, HasLabel):
    pass


class OsAtom(HasIRI):
    X: HasValueHasUnit
    Y: HasValueHasUnit
    Z: HasValueHasUnit
