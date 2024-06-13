from functools import cache
from typing_extensions import TypedDict

from pydantic import BaseModel, ConfigDict, Field, TypeAdapter
from pydantic.fields import FieldInfo
from rdflib import URIRef
from rdflib.paths import Path


class RDFFieldMetadata(TypedDict):
    path: URIRef | Path


setattr(
    RDFFieldMetadata, "__pydantic_config__", ConfigDict(arbitrary_types_allowed=True)
)


class RDFEntity(BaseModel):
    IRI: str

    @classmethod
    @cache
    def get_rdf_fields(cls):
        adapter = TypeAdapter(RDFFieldMetadata)
        fields: dict[str, tuple[FieldInfo, RDFFieldMetadata]] = dict()
        for field, info in cls.model_fields.items():
            if field == "IRI":
                continue
            try:
                rdf_metadata = adapter.validate_python(info.json_schema_extra)
            except:
                continue
            fields[field] = (info, rdf_metadata)
        return fields


def RDFField(
    path: URIRef | Path,
    **kwargs,
):
    return Field(**kwargs, json_schema_extra=RDFFieldMetadata(path=path))
