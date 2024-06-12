from typing import Sequence

from services.kg import KgClient


def get_labels(kg_client: KgClient, iris: Sequence[str]):
    query = """SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI rdfs:label ?Label .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )
    _, bindings = kg_client.querySelectThenFlatten(query)
    iri2label = {binding["IRI"]: binding["Label"] for binding in bindings}
    return [iri2label.get(iri) for iri in iris]
