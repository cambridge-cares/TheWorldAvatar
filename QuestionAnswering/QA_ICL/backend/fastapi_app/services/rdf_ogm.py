from types import NoneType, UnionType
from typing import (
    Any,
    TypeVar,
    get_origin,
    get_args,
)
from collections import defaultdict

from pydantic.fields import FieldInfo
from rdflib import URIRef

from model.rdf_ogm import RDFEntity
from services.sparql import SparqlClient
from utils.itertools_recipes import batched


T = TypeVar("T", bound=RDFEntity)


def unpack_optional_type(annotation: type[Any]):
    if get_origin(annotation) is UnionType:
        args = get_args(annotation)
        return next(arg for arg in args if arg is not NoneType)
    else:
        return annotation


class RDFStore:
    BATCH_SIZE = 256

    def __init__(self, endpoint: str):
        self.sparql_client = SparqlClient(endpoint)

    def _get_many(
        self, T: type[T], iris: list[str] | tuple[str], return_fields: list[str] | None
    ):
        return_fields = None if return_fields is None else set(return_fields)

        if not iris:
            empty_lst: list[T | None] = []
            return empty_lst

        query = """SELECT ?iri ?field ?value
WHERE {{
    VALUES ?iri {{ {iris} }}
    {triples}
}}""".format(
            iris=" ".join("<{iri}>".format(iri=iri) for iri in set(iris)),
            triples=" UNION ".join(
                """{{
    BIND ( "{field}" as ?field )
    ?iri {predicate} ?value .
}}""".format(
                    field=field,
                    predicate=metadata["path"].n3(),
                )
                for field, metadata in T.get_rdf_fields().items()
                if return_fields is None or field in return_fields
            ),
        )

        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        field2iri2values: dict[str, defaultdict[str, list[str]]] = defaultdict(
            lambda: defaultdict(list)
        )
        for binding in bindings:
            if "field" not in binding or "value" not in binding:
                continue
            field2iri2values[binding["field"]][binding["iri"]].append(binding["value"])

        def resolve_field_value_batch(field: str, info: FieldInfo):
            annotation = info.annotation
            if not annotation:
                pass
            else:       
                def list_values_process(t):
                    iri2values = field2iri2values[field]    
                    if issubclass(t, RDFEntity):
                        flattened = [v for values in iri2values.values() for v in values]
                        models = [x for x in self.get_many(t, flattened) if x]
                        count = 0
                        out: dict[str, list[RDFEntity]] = dict()
                        for iri, iri2values in iri2values.items():
                            out[iri] = models[count : count + len(iri2values)]
                            count += len(iri2values)
                        return out
                    else:
                        return iri2values
                    
                origin = get_origin(annotation)
                args = get_args(annotation) 
                   
                if origin == list:
                    return list_values_process(args[0])
                else:
                    for arg in args:
                        if get_origin(arg) is list:
                            return list_values_process(get_args(arg)[0])

            iri2values = field2iri2values[field]
            iri2value = {
                iri: values[0] if values else None for iri, values in iri2values.items()
            }

            if annotation:
                unpacked_type = unpack_optional_type(annotation)
                try:
                    if issubclass(unpacked_type, RDFEntity):
                        models = self.get_many(unpacked_type, iri2value.values())
                        return {
                            iri: model for iri, model in zip(iri2value.keys(), models)
                        }
                except:
                    pass
            return iri2value

        field2iri2data = {
            field: resolve_field_value_batch(field, info)
            for field, info in T.model_fields.items()
            if field != "IRI" and (return_fields is None or field in return_fields)
        }

        iri2field2data: defaultdict[
            str, dict[str, RDFEntity | str | list[RDFEntity] | list[str]]
        ] = defaultdict(dict)
        for field, iri2data in field2iri2data.items():
            for iri, data in iri2data.items():
                iri2field2data[iri][field] = data

        def resolve_field_value(
            model_fields: dict[str, FieldInfo],
            field2data: dict[str, RDFEntity | str | list[RDFEntity] | list[str]],
        ):
            if all(
                field in field2data for field in model_fields.keys() if field != "IRI"
            ):
                return field2data

            data = dict(field2data)
            for field, info in model_fields.items():
                if field == "IRI" or field in data:
                    continue
                annotation = info.annotation
                if annotation:
                    if get_origin(annotation) is list:
                        data[field] = list()
                    else:
                        try:
                            if NoneType in get_args(annotation):
                                data[field] = None
                        except:
                            pass
            return data

        def parse(data: dict):
            try:
                return T.model_validate(data)
            except:
                return None

        return [
            parse(
                {
                    **resolve_field_value(
                        model_fields=T.model_fields,
                        field2data=iri2field2data[iri],
                    ),
                    "IRI": iri,
                }
            )
            for iri in iris
        ]

    def get_many(
        self,
        T: type[T],
        iris: list[str] | tuple[str],
        return_fields: list[str] | None = None,
    ):
        """
        If `return_fields` is specified, all fields on `T` must be of `Optional` type.
        """
        return [
            x
            for batch in batched(iris, self.BATCH_SIZE)
            for x in self._get_many(T=T, iris=batch, return_fields=return_fields)
        ]

    def get_one(self, T: type[T], iri: str):
        return self.get_many(T, [iri])[0]

    def get_all(self, T: type[T], type_iri: URIRef):
        query = f"""SELECT DISTINCT ?IRI
WHERE {{
    ?IRI rdf:type {type_iri.n3()} .
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [binding["IRI"] for binding in bindings]
        return [model for model in self.get_many(T, iris) if model]

    def _resolve_sparql_client(self, sparql_client: str | SparqlClient | None):
        if sparql_client is None:
            return self.sparql_client
        if isinstance(sparql_client, SparqlClient):
            return sparql_client
        return SparqlClient(sparql_client)
