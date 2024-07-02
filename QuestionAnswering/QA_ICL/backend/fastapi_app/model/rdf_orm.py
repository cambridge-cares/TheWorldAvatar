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


adapter = TypeAdapter(RDFFieldMetadata)


def _get_rdf_field_metadata(info: FieldInfo):
    try:
        return adapter.validate_python(info.metadata[0])
    except:
        return None


class RDFEntity(BaseModel):
    model_config = ConfigDict(frozen=True)

    IRI: str

    @classmethod
    @cache
    def get_rdf_fields(cls):
        return {
            field: (info, rdf_meta)
            for field, (info, rdf_meta) in {
                field: (info, _get_rdf_field_metadata(info))
                for field, info in cls.model_fields.items()
                if field != "IRI"
            }.items()
            if rdf_meta
        }


def RDFField(
    path: URIRef | Path,
    **kwargs,
):
    field_info = Field(**kwargs)
    field_info.metadata.append(RDFFieldMetadata(path=path))
    return field_info
