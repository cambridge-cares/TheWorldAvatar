from typing import List, Optional

from pydantic import BaseModel

RELATIONS_KEY_PREFIX = "relations:"
RELATIONS_INDEX_NAME = "idx:relations_vss"


class RDFItemAnnotation(BaseModel):
    label: Optional[str] = None
    comment: Optional[str] = None


class RDFItemAnnotated(BaseModel):
    iri: str
    annotation: RDFItemAnnotation


class RDFRelation(BaseModel):
    s: str
    p: str
    o: str


class RDFRelationProcessed(RDFRelation):
    triple_repr: str
    triple_repr_embedding: List[float]
