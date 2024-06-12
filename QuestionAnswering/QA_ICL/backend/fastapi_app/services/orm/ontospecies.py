from typing import Sequence
from pydantic import TypeAdapter

from model.orm.base import HasValueHasUnit
from model.orm.ontospecies import (
    OsChemicalClass,
    OsIdentifier,
    OsProperty,
    OsSpecies,
    OsUse,
)
from services.kg import KgClient
from services.orm.base import get_labels


class OntoSpeciesStore:
    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client
        self.species_identifiers_lst_adapter = TypeAdapter(list[OsSpecies])
        self.property_lst_adapter = TypeAdapter(list[OsProperty])
        self.identifier_lst_adapter = TypeAdapter(list[OsIdentifier])

    def get_species_unique_identifiers(self, iris: Sequence[str]):
        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        
SELECT *
WHERE {{
    {{
        VALUES ?IRI {{ {values} }}
        ?IRI rdfs:label ?Label .
    }}
    OPTIONAL {{
        SELECT ?IRI (SAMPLE(?IUPACName) AS ?IUPACName)
        WHERE {{
            VALUES ?IRI {{ {values} }}
            ?IRI os:hasIUPACName/os:value ?IUPACName .
        }}
        GROUP BY ?IRI
    }} 
    {{
        VALUES ?IRI {{ {values} }}
        ?IRI os:hasInChI/os:value ?InChI .
    }}
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        data = self.species_identifiers_lst_adapter.validate_python(bindings)
        iri2datum = {datum.IRI: datum for datum in data}
        return [iri2datum.get(iri) for iri in iris]

    def get_property(self, iris: Sequence[str]):
        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        
SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI os:value ?Value .
    OPTIONAL {{
        ?IRI os:unit/rdfs:label ?Unit .
    }}
    OPTIONAL {{
        ?IRI os:hasReferenceState [ os:value ?RefStateValue ; os:unit/rdfs:label ?RefStateUnit ] .
    }}
    OPTIONAL {{
        ?IRI os:hasProvenance/rdfs:label ?Provenance
    }}
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        ref_states = [
            {
                k[len("RefState") :]: v
                for k, v in binding.items()
                if k.startswith("RefState")
            }
            for binding in bindings
        ]
        ref_states = [
            HasValueHasUnit.model_validate(x) if x else None for x in ref_states
        ]
        data = [
            OsProperty.model_validate(
                {**binding, "RefState": ref_state} if ref_state else binding
            )
            for binding, ref_state in zip(bindings, ref_states)
        ]
        data = self.property_lst_adapter.validate_python(bindings)
        iri2datum = {datum.IRI: datum for datum in data}
        return [iri2datum.get(iri) for iri in iris]

    def get_identifier(self, iris: Sequence[str]):
        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        
SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI os:value ?Value .
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        data = self.identifier_lst_adapter.validate_python(bindings)
        iri2datum = {datum.iri: datum for datum in data}
        return [iri2datum.get(iri) for iri in iris]

    def _get_label(self, iris: Sequence[str]):
        query = """SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI rdfs:label ?Label .
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        iri2binding = {binding["IRI"]: binding for binding in bindings}
        return [iri2binding.get(iri) for iri in iris]

    def get_chemical_class(self, iris: Sequence[str]):
        labels = get_labels(kg_client=self.bg_client, iris=iris)
        return [
            OsChemicalClass(IRI=iri, Label=label) for iri, label in zip(iris, labels)
        ]

    def get_use(self, iris: Sequence[str]):
        labels = get_labels(kg_client=self.bg_client, iris=iris)
        return [OsUse(IRI=iri, Label=label) for iri, label in zip(iris, labels)]
