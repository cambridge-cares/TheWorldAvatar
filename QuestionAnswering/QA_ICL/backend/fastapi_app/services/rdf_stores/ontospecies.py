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
    OntospeciesSpeciesBase,
    OntospeciesUse,
    PeriodictableElement,
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
            "os:Species": self.get_species_many,
            "os:Property": self.get_properties_many,
            "os:Identifier": self.get_identifiers_many,
            "os:ChemicalClass": self.get_chemical_classes_many,
            "os:Use": self.get_uses_many,
        }

    def get_elements_many(self, iris: list[str] | tuple[str]):
        return self.getMany(PeriodictableElement, iris)

    def get_atoms_many(self, iris: list[str] | tuple[str]):
        return self.getMany(GcAtom, iris)

    def get_species_many(self, iris: list[str] | tuple[str]):
        return self.getMany(OntospeciesSpeciesBase, iris)

    def get_species(self, req: SpeciesRequest):
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
        species = self.get_species_many([binding["Species"] for binding in bindings])
        return [x for x in species if x]

    def get_properties_many(self, iris: list[str] | tuple[str]):
        return self.getMany(OntospeciesProperty, iris)

    def get_identifiers_many(self, iris: list[str] | tuple[str]):
        return self.getMany(OntospeciesIdentifier, iris)

    def get_chemical_classes_many(self, iris: list[str] | tuple[str]):
        return self.getMany(OntospeciesChemicalClass, iris)

    def get_chemical_classes_all(self):
        return self.getAll(OntospeciesChemicalClass, ONTOSPECIES.ChemicalClass)

    def get_uses_many(self, iris: list[str] | tuple[str]):
        return self.getMany(OntospeciesUse, iris)

    def get_uses_all(self):
        return self.getAll(OntospeciesUse, ONTOSPECIES.Use)


@cache
def get_ontospecies_rdfStore(
    endpoint: Annotated[str, Depends(get_ontospecies_endpoint)]
):
    return OntospeciesRDFStore(endpoint)
