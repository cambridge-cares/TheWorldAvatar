from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends
from services.kg import KgClient, get_ontospecies_bgClient
from services.processs_response.augment_node import NodeDataRetriever


# TODO: ORM
def get_species_unique_identifiers(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
SELECT *
WHERE {{
    {{
        VALUES ?Species {{ {values} }}
        ?Species rdfs:label ?Label .
    }}
    OPTIONAL {{
        SELECT ?Species (SAMPLE(?IUPACName) AS ?IUPACName)
        WHERE {{
            VALUES ?Species {{ {values} }}
            ?Species os:hasIUPACName/os:value ?IUPACName .
        }}
        GROUP BY ?Species
    }} 
    {{
        VALUES ?Species {{ {values} }}
        ?Species os:hasInChI/os:value ?InChI .
    }}
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings  = kg_client.querySelectThenFlatten(query)

    iri2data = {
        binding["Species"]: {k: binding.get(k) for k in ["Label", "IUPACName", "InChI"]}
        for binding in bindings
    }
    iri2data = {
        iri: {k: v for k, v in data.items() if v} for iri, data in iri2data.items()
    }

    return [{"IRI": iri, **iri2data.get(iri, {})} for iri in iris]


def get_ontospecies_property_data(kg_client: KgClient, iris: Sequence[str]):
    query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    
SELECT *
WHERE {{
    VALUES ?Property {{ {values} }}
    ?Property os:value ?Value .
    OPTIONAL {{
        ?Property os:unit/rdfs:label ?Unit .
    }}
    OPTIONAL {{
        ?Property os:hasReferenceState [ os:value ?RefStateValue ; os:unit/rdfs:label ?RefStateUnit ] .
    }}
    OPTIONAL {{
        ?Property os:hasProvenance/rdfs:label ?Provenance
    }}
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings  = kg_client.querySelectThenFlatten(query)

    iri2data = {
        binding["Property"]: {
            k: binding.get(k)
            for k in ["Value", "Unit", "RefStateValue", "RefStateUnit", "Provenance"]
        }
        for binding in bindings
    }
    iri2data = {
        iri: {k: v for k, v in data.items() if v} for iri, data in iri2data.items()
    }

    return [{"IRI": iri, **iri2data.get(iri, {})} for iri in iris]


def get_label(kg_client: KgClient, iris: Sequence[str]):
    query = """SELECT *
WHERE {{
    VALUES ?s {{ {values} }}
    ?s rdfs:label ?Label .
}}""".format(
        values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
    )

    _, bindings  = kg_client.querySelectThenFlatten(query)

    iri2data = {binding["s"]: {"Label": binding["Label"]} for binding in bindings}
    return [{"IRI": iri, **iri2data.get(iri, {})} for iri in iris]


@cache
def get_ontospecies_nodeDataRetriever(
    bg_client: Annotated[KgClient, Depends(get_ontospecies_bgClient)]
):
    return NodeDataRetriever(
        kg_client=bg_client,
        type2getter={
            "os:Species": get_species_unique_identifiers,
            "os:Property": get_ontospecies_property_data,
            "os:ChemicalClass": get_label,
            "os:Use": get_label,
        },
    )
