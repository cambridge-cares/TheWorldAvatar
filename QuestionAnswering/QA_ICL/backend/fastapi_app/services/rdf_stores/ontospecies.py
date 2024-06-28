from functools import cache
import itertools
from typing import Annotated

from fastapi import Depends

from constants.namespace import ONTOSPECIES
from model.comp_op import COMP_OP_2_SPARQL_SYMBOL
from model.kg.ontospecies import (
    GcAtom,
    OntospeciesChemicalClass,
    OntospeciesIdentifier,
    OntospeciesProperty,
    OntospeciesSpeciesBase,
    OntospeciesUse,
    PeriodictableElement,
)
from model.ontospecies import SpeciesRequest
from services.rdf_orm import RDFStore
from services.rdf_stores.base import Cls2GetterRDFStore
from services.sparql import SparqlClient, get_ontospecies_endpoint


class OntospeciesRDFStore(Cls2GetterRDFStore):
    def __init__(self, ontospecies_endpoint: str):
        self.rdf_store = RDFStore(ontospecies_endpoint)
        self.sparql_client = SparqlClient(ontospecies_endpoint)

    @property
    def cls2getter(self):
        return {
            "pt:Element": self.get_elements_many,
            "gc:Atom": self.get_atoms_many,
            "os:Species": self.get_species_many,
            "os:Property": self.get_properties_many,
            "os:Identifier": self.get_identifiers_many,
            "os:ChemicalClass": self.get_chemical_classes_many,
            "os:Use": self.get_uses_many,
        }

    def get_elements_many(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(PeriodictableElement, iris)

    def get_atoms_many(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(GcAtom, iris)

    def get_species_many(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntospeciesSpeciesBase, iris)

    def get_species(self, req: SpeciesRequest):
        chemclass_triples = [
            f"?Species os:hasChemicalClass/rdfs:subClassOf* <{iri}> ."
            for iri in req.chemical_class
        ]
        use_triples = [f"?Species os:hasUse <{iri}> ." for iri in req.use]

        property_triple_pairs = [
            [
                f"?Species os:has{key}/os:value ?{key}Value .",
                "FILTER ( {} )".format(
                    " && ".join(
                        f"?{key}Value {COMP_OP_2_SPARQL_SYMBOL[op]} {rhs}"
                        for op, rhs in conds
                    )
                ),
            ]
            for key, conds in req.property.items()
            if conds
        ]
        property_triples = [triple for pair in property_triple_pairs for triple in pair]

        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Species
WHERE {{
    ?Species a os:Species .
    {}
}}""".format(
            "\n    ".join(
                itertools.chain(chemclass_triples, use_triples, property_triples)
            )
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        species = self.get_species_many([binding["Species"] for binding in bindings])
        return [x for x in species if x]

    def get_properties_many(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntospeciesProperty, iris)

    def get_identifiers_many(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntospeciesIdentifier, iris)

    def get_chemical_classes_many(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntospeciesChemicalClass, iris)

    def get_chemical_classes_all(self):
        return self.rdf_store.getAll(
            OntospeciesChemicalClass, ONTOSPECIES.ChemicalClass
        )

    def get_uses_many(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntospeciesUse, iris)


@cache
def get_ontospecies_rdfStore(
    endpoint: Annotated[str, Depends(get_ontospecies_endpoint)]
):
    return OntospeciesRDFStore(endpoint)
