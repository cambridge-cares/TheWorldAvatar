from collections import defaultdict
from functools import cache
import itertools
from typing import Annotated

from fastapi import Depends

from constants.namespace import ONTOSPECIES
from model.web.comp_op import COMP_OP_2_SPARQL_SYMBOL
from model.kg.ontospecies import (
    GcAtom,
    OntospeciesChemicalClass,
    OntospeciesIdentifier,
    OntospeciesProperty,
    OntospeciesSpecies,
    OntospeciesSpeciesBase,
    OntospeciesUse,
    PeriodictableElement,
    SpeciesIdentifierKey,
    SpeciesPropertyKey,
)
from model.web.ontospecies import SpeciesRequest
from services.rdf_orm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import get_ontospecies_endpoint


class OntospeciesRDFStore(Cls2NodeGetter, RDFStore):
    @property
    def cls2getter(self):
        return {
            "pt:Element": self.get_elements_many,
            "gc:Atom": self.get_atoms_many,
            "os:Species": self.get_species_base_many,
            "os:Property": self.get_properties_many,
            "os:Identifier": self.get_identifiers_many,
            "os:ChemicalClass": self.get_chemical_classes_many,
            "os:Use": self.get_uses_many,
        }

    def get_elements_many(self, iris: list[str] | tuple[str]):
        return self.get_many(PeriodictableElement, iris)

    def get_atoms_many(self, iris: list[str] | tuple[str]):
        return self.get_many(GcAtom, iris)

    def get_species_base_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntospeciesSpeciesBase, iris)

    def get_species_base(self, req: SpeciesRequest):
        chemclass_patterns = [
            f"?Species os:hasChemicalClass/rdfs:subClassOf* <{iri}> ."
            for iri in req.chemical_class
        ]
        use_patterns = [f"?Species os:hasUse <{iri}> ." for iri in req.use]

        identifier_patterns = [
            f'?Species os:has{key}/os:value "{value}" .'
            for key, value in req.identifier.items()
        ]

        property_patterns = [
            pattern
            for key, conds in req.property.items()
            if conds
            for pattern in (
                f"?Species os:has{key}/os:value ?{key}Value .",
                "FILTER ( {} )".format(
                    " && ".join(
                        f"?{key}Value {COMP_OP_2_SPARQL_SYMBOL[op]} {rhs}"
                        for op, rhs in conds
                    )
                ),
            )
        ]

        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Species
WHERE {{
    ?Species a os:Species .
    {}
}}""".format(
            "\n    ".join(
                itertools.chain(
                    chemclass_patterns,
                    use_patterns,
                    identifier_patterns,
                    property_patterns,
                )
            )
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        species = self.get_species_base_many(
            [binding["Species"] for binding in bindings]
        )
        return [x for x in species if x]

    def get_species_one(self, iri: str):
        species = self.get_species_base_many(iris=[iri])[0]
        if species is None:
            return None

        query = """PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?field ?value
WHERE {{
    VALUES (?field ?p) {{ {pairs} }}
    <{iri}> ?p ?value .
}}""".format(
            iri=iri,
            pairs=" ".join(
                [
                    f'("{field}" {p})'
                    for field, p in [
                        ("altLabel", "skos:altLabel"),
                        ("chemicalClass", "os:hasChemicalClass"),
                        ("use", "os:hasUse"),
                        *((key.value, f"os:has{key}") for key in SpeciesIdentifierKey),
                        *((key.value, f"os:has{key}") for key in SpeciesPropertyKey),
                    ]
                ]
            ),
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        field2values: defaultdict[str, list[str]] = defaultdict(list)
        for binding in bindings:
            field2values[binding["field"]].append(binding["value"])

        alt_labels = field2values["altLabel"]
        chemical_classes = self.get_chemical_classes_many(field2values["chemicalClass"])
        uses = self.get_uses_many(field2values["use"])

        identifier_key2iris = {
            key: field2values[key.value] for key in SpeciesIdentifierKey
        }
        identifier_iris = list(itertools.chain(*identifier_key2iris.values()))
        identifier_models = self.get_identifiers_many(identifier_iris)
        identifier_iri2model = {
            iri: model for iri, model in zip(identifier_iris, identifier_models)
        }
        identifiers = {
            key: [x for x in [identifier_iri2model[iri] for iri in iris] if x]
            for key, iris in identifier_key2iris.items()
        }

        property_key2iris = {key: field2values[key.value] for key in SpeciesPropertyKey}
        property_iris = list(itertools.chain(*property_key2iris.values()))
        property_models = self.get_properties_many(iris=property_iris)
        property_iri2model = {
            iri: model for iri, model in zip(property_iris, property_models)
        }
        properties = {
            key: [x for x in [property_iri2model[iri] for iri in iris] if x]
            for key, iris in property_key2iris.items()
        }

        return OntospeciesSpecies(
            **species.model_dump(),
            alt_labels=alt_labels,
            chemical_classes=chemical_classes,
            uses=uses,
            identifiers=identifiers,
            properties=properties,
        )

    def get_properties_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntospeciesProperty, iris)

    def get_identifiers_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntospeciesIdentifier, iris)

    def get_chemical_classes_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntospeciesChemicalClass, iris)

    def get_chemical_classes_all(self):
        return self.get_all(OntospeciesChemicalClass, ONTOSPECIES.ChemicalClass)

    def get_uses_many(self, iris: list[str] | tuple[str]):
        return self.get_many(OntospeciesUse, iris)

    def get_uses_all(self):
        return self.get_all(OntospeciesUse, ONTOSPECIES.Use)


@cache
def get_ontospecies_rdfStore(
    endpoint: Annotated[str, Depends(get_ontospecies_endpoint)]
):
    return OntospeciesRDFStore(endpoint)
