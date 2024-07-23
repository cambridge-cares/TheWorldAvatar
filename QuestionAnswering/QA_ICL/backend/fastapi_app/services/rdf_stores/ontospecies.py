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
from model.web.ontospecies import SpeciesReqReturnFields, SpeciesRequest
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

    def get_species_IRIs(self, req: SpeciesRequest):
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
        return [binding["Species"] for binding in bindings]

    def get_species_fields(
        self, iris: list[str], return_fields: SpeciesReqReturnFields | None = None
    ):
        species_base = self.get_species_base_many(iris=iris)

        field_pred_pairs: list[tuple[str, str]] = [
            pair
            for pair in [
                (
                    ("altLabel", "skos:altLabel")
                    if return_fields is None or return_fields.alt_label
                    else None
                ),
                (
                    (
                        "chemicalClass",
                        "os:hasChemicalClass",
                    )  # no traversal of chemical class graphs
                    if return_fields is None or return_fields.chemical_class
                    else None
                ),
                (
                    ("use", "os:hasUse")
                    if return_fields is None or return_fields.use
                    else None
                ),
                *(
                    (key.value, f"os:has{key}")
                    for key in (
                        return_fields.identifier
                        if return_fields
                        else SpeciesIdentifierKey
                    )
                ),
                *(
                    (key.value, f"os:has{key}")
                    for key in (
                        return_fields.property if return_fields else SpeciesPropertyKey
                    )
                ),
            ]
            if pair
        ]

        if not field_pred_pairs:
            return [
                (
                    OntospeciesSpecies(
                        **base_model.model_dump(),
                        altLabel=list(),
                        ChemicalClass=list(),
                        Use=list(),
                        Property=dict(),
                        Identifier=dict(),
                    )
                    if base_model
                    else None
                )
                for base_model in species_base
            ]

        query = """PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Species ?field ?value
WHERE {{
    VALUES ?Species {{ {IRIs} }}
    {patterns}
}}""".format(
            IRIs=" ".join(f"<{iri}>" for iri in iris),
            patterns=" UNION ".join(
                f"""{{
    BIND ( "{field}" as ?field )
    ?Species {pred} ?value .
}}"""
                for field, pred in field_pred_pairs
            ),
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        field2iri2values: defaultdict[str, defaultdict[str, list[str]]] = defaultdict(
            lambda: defaultdict(list)
        )
        for binding in bindings:
            field2iri2values[binding["field"]][binding["Species"]].append(
                binding["value"]
            )

        speciesIRI_to_chemclassIRIs = field2iri2values["chemicalClass"]
        chemical_classes = (
            x
            for x in self.get_chemical_classes_many(
                list(itertools.chain(*speciesIRI_to_chemclassIRIs.values()))
            )
        )
        iri2chemclass = {
            iri: [x for x in [next(chemical_classes) for _ in chemclass_iris] if x]
            for iri, chemclass_iris in speciesIRI_to_chemclassIRIs.items()
        }

        speciesIRI_to_useIRIs = field2iri2values["use"]
        uses = (
            x
            for x in self.get_uses_many(
                list(itertools.chain(*speciesIRI_to_useIRIs.values()))
            )
        )
        iri2use = {
            iri: [x for x in [next(uses) for _ in use_iris] if x]
            for iri, use_iris in speciesIRI_to_useIRIs.items()
        }

        identifier_speciesIRI_to_nodeIRIs = {
            key: field2iri2values[key.value] for key in SpeciesIdentifierKey
        }
        identifiers = (
            x
            for x in self.get_identifiers_many(
                list(
                    itertools.chain(
                        *(
                            itertools.chain(*speciesIRI_to_keyIRIs.values())
                            for speciesIRI_to_keyIRIs in identifier_speciesIRI_to_nodeIRIs.values()
                        )
                    )
                )
            )
        )
        identifier_iri2nodes = {
            key: {
                speciesIRI: [x for x in [next(identifiers) for _ in nodeIRIs] if x]
                for speciesIRI, nodeIRIs in speciesIRI_to_nodeIRIs.items()
            }
            for key, speciesIRI_to_nodeIRIs in identifier_speciesIRI_to_nodeIRIs.items()
        }
        iri2identifiers: defaultdict[
            str, dict[SpeciesIdentifierKey, list[OntospeciesIdentifier]]
        ] = defaultdict(dict)
        for key, iri2nodes in identifier_iri2nodes.items():
            for iri, nodes in iri2nodes.items():
                iri2identifiers[iri][key] = nodes

        property_speciesIRI_to_nodeIRIs = {
            key: field2iri2values[key.value]
            for key in SpeciesPropertyKey
            if key.value in field2iri2values
        }
        properties = (
            x
            for x in self.get_properties_many(
                list(
                    itertools.chain(
                        *(
                            itertools.chain(*speciesIRI_to_keyIRIs.values())
                            for speciesIRI_to_keyIRIs in property_speciesIRI_to_nodeIRIs.values()
                        )
                    )
                )
            )
        )
        property_iri2nodes = {
            key: {
                speciesIRI: [x for x in [next(properties) for _ in nodeIRIs] if x]
                for speciesIRI, nodeIRIs in speciesIRI_to_nodeIRIs.items()
            }
            for key, speciesIRI_to_nodeIRIs in property_speciesIRI_to_nodeIRIs.items()
        }
        iri2properties: defaultdict[
            str, dict[SpeciesPropertyKey, list[OntospeciesProperty]]
        ] = defaultdict(dict)
        for key, iri2nodes in property_iri2nodes.items():
            for iri, nodes in iri2nodes.items():
                iri2properties[iri][key] = nodes

        return [
            (
                OntospeciesSpecies(
                    **base_model.model_dump(),
                    altLabel=field2iri2values["altLabel"][iri],
                    ChemicalClass=iri2chemclass.get(iri, list()),
                    Use=iri2use.get(iri, list()),
                    Identifier=iri2identifiers[iri],
                    Property=iri2properties[iri],
                )
                if base_model
                else None
            )
            for iri, base_model in zip(iris, species_base)
        ]

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
