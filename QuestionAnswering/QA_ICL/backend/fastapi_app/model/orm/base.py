from pydantic import BaseModel


class HasIRI(BaseModel):
    IRI: str


class HasValue(BaseModel):
    Value: str | float


class HasUnit(BaseModel):
    Unit: str


class HasValueHasUnit(HasValue, HasUnit):
    pass


class HasUnitOptional(BaseModel):
    Unit: str | None


class HasLabel(BaseModel):
    Label: str


class HasProvenance(BaseModel):
    Provenance: str
