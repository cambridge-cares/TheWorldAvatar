from types import NoneType, UnionType
from typing import (
    Any,
    Sequence,
    TypeVar,
    get_origin,
    get_args,
)
from collections import defaultdict

from pydantic import TypeAdapter
from pydantic.fields import FieldInfo

from model.rdf_orm import RDFEntity
from services.sparql import SparqlClient


T = TypeVar("T", bound=RDFEntity)


def unpack_optional_type(annotation: type[Any]):
    if get_origin(annotation) is UnionType:
        args = get_args(annotation)
        return next(arg for arg in args if arg is not NoneType)
    else:
        return annotation


class RDFStore:
    def __init__(self, endpoint: str):
        self.sparql_client = SparqlClient(endpoint)

    def getMany(self, T: type[T], iris: Sequence[str]):
        unique_iris = list(set(iris))

        query = """SELECT *
WHERE {{
    VALUES ?iri {{ {iris} }}
    {triples}
}}""".format(
            iris=" ".join("<{iri}>".format(iri=iri) for iri in unique_iris),
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

        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        field2iri2values: dict[str, defaultdict[str, list[str]]] = defaultdict(
            lambda: defaultdict(list)
        )
        for binding in bindings:
            field2iri2values[binding["field"]][binding["iri"]].append(binding["value"])

        def resolve_field_value(field: str, info: FieldInfo):
            annotation = info.annotation
            if annotation and get_origin(annotation) is list:
                t = get_args(annotation)[0]
                iri2values = field2iri2values[field]
                if issubclass(t, RDFEntity):
                    flattened = [v for values in iri2values.values() for v in values]
                    models = self.getMany(t, flattened)
                    count = 0
                    out: dict[str, list[RDFEntity]] = dict()
                    for iri, iri2values in iri2values.items():
                        out[iri] = models[count : count + len(iri2values)]
                        count += len(iri2values)
                    return out
                else:
                    return iri2values

            iri2values = field2iri2values[field]
            iri2value = {
                iri: values[0] if values else None for iri, values in iri2values.items()
            }

            if annotation:
                unpacked_type = unpack_optional_type(annotation)
                if issubclass(unpacked_type, RDFEntity):
                    models = self.getMany(unpacked_type, iri2value.values())
                    return {iri: model for iri, model in zip(iri2value.keys(), models)}
            return iri2value

        field2iri2data = {
            field: resolve_field_value(field, info)
            for field, (info, _) in T.get_rdf_fields().items()
        }

        iri2field2data = defaultdict(dict)
        for field, iri2data in field2iri2data.items():
            for iri, data in iri2data.items():
                iri2field2data[iri][field] = data

        adapter = TypeAdapter(list[T])
        models = adapter.validate_python(
            [{"IRI": iri, **field2data} for iri, field2data in iri2field2data.items()]
        )
        iri2model = {model.IRI: model for model in models}
        return [iri2model.get(iri) for iri in iris]

    def getOne(self, T: type[T], iri: str):
        return self.getMany(T, [iri])[0]
