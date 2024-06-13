from functools import cache
from types import NoneType, UnionType
from typing import Any, TypeVar, TypedDict, get_origin, get_args
from collections import defaultdict

from pydantic import BaseModel, Field
from pydantic.fields import FieldInfo
from rdflib import URIRef
from rdflib.paths import Path
from SPARQLWrapper import SPARQLWrapper, JSON


class RDFFieldMetadata(TypedDict):
    path: URIRef | Path


class RDFEntity(BaseModel):
    IRI: str

    @classmethod
    @cache
    def get_rdf_fields(self):
        return {
            k: (v, RDFFieldMetadata(v.json_schema_extra))
            for k, v in self.model_fields.items()
            if k != "IRI"
        }


def RDFField(
    path: URIRef | Path,
    **kwargs,
):
    return Field(**kwargs, json_schema_extra=RDFFieldMetadata(path=path))


T = TypeVar("T", bound=RDFEntity)


def issubclass_of_rdf_entity(annotation: type[Any]):
    if get_origin(annotation) is UnionType:
        args = get_args(annotation)
        try:
            return any(
                issubclass(arg, RDFEntity) for arg in args if arg is not NoneType
            )
        except:
            return False
    else:
        try:
            return issubclass(annotation, RDFEntity)
        except:
            return False


class RDFStore:
    def __init__(self, endpoint: str):
        client = SPARQLWrapper(endpoint)
        client.setReturnFormat(JSON)
        self.client = client

    def get(self, T: type[T], iri: str):
        query = """SELECT *
WHERE {{
    VALUES ?iri {{ <{iri}> }}
    {triples}
}}""".format(
            iri=iri,
            triples=" UNION ".join(
                """{{
    BIND ( "{field}" as ?field )
    ?iri {predicate} ?value .
}}""".format(
                    field=field, predicate=metadata["path"].n3()
                )
                for field, (_, metadata) in T.get_rdf_fields().items()
            ),
        )

        self.client.setQuery(query)
        bindings = self.client.queryAndConvert()["results"]["bindings"]
        bindings = [{k: v["value"] for k, v in binding.items()} for binding in bindings]
        data: defaultdict[str, list[str]] = defaultdict(list)
        for binding in bindings:
            data[binding["field"]].append(binding["value"])

        def resolve_field_value(field: str, info: FieldInfo):
            annotation = info.annotation
            if annotation and get_origin(annotation) is list:
                t = get_args(annotation)[0]
                if issubclass(t, RDFEntity):
                    return [self.get(t, v) for v in data[field]]
                else:
                    return data[field]

            values = data[field]
            if values:
                value = values[0]
            else:
                value = None

            if annotation and issubclass_of_rdf_entity(annotation):
                return self.get(annotation, value)
            else:
                return value

        model_data = {
            field: resolve_field_value(field, info)
            for field, (info, _) in T.get_rdf_fields().items()
        }

        return T.model_validate({"IRI": iri, **model_data})
