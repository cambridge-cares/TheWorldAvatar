from pydantic import BaseModel

PROPERTIES_KEY_PREFIX = "properties:"
PROPERTIES_INDEX_NAME = "idx:properties_vss"


class RDFProperty(BaseModel):
    iri: str
    label: str | None = None
    comment: str | None = None


class RDFPropertyProcessed(RDFProperty):
    embedding: list[float]
