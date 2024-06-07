from typing import Sequence
from services.kg import KgClient


def get_mechanism_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
PREFIX op: <http://www.theworldavatar.com/ontology/ontoprovenance/OntoProvenance.owl#>

SELECT ?Mechanism ?Provenance
WHERE {{
    VALUES ?Mechanism {{ {values} }}
    ?Mechanism okin:hasProvenance/(op:hasDOI|op:hasURL) ?Provenance .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings  = kg_client.querySelectThenFlatten(query)

    iri2data = {
        binding["Mechanism"]: {"Provenance": binding["Provenance"]}
        for binding in bindings
    }
    return [iri2data.get(iri, {}) for iri in iris]
