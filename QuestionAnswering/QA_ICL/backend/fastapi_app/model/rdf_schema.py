from pydantic import BaseModel

RELATIONS_KEY_PREFIX = "relations:"
RELATIONS_INDEX_NAME = "idx:relations_vss"

PROPERTIES_KEY_PREFIX = "properties:"
PROPERTIES_INDEX_NAME = "idx:properties_vss"


class RDFItemAnnotation(BaseModel):
    label: str | None = None
    comment: str | None = None


class RDFItemAnnotated(BaseModel):
    iri: str
    annotation: RDFItemAnnotation


class RDFRelation(BaseModel):
    s: str
    p: str
    o: str


class RDFRelationProcessed(RDFRelation):
    triple_repr: str
    triple_repr_embedding: list[float]


class RDFProperty(BaseModel):
    iri: str
    label: str | None = None
    comment: str | None = None


class RDFPropertyProcessed(RDFProperty):
    embedding: list[float]
