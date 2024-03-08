import logging
from typing import Annotated, Dict, List, Tuple

from fastapi import Depends

from model.constraint import CompoundNumericalConstraint
from services.kg_client import KgClient
from .kg_client import get_ontospecies_kg_client
from .constants import (
    SpeciesAttrKey,
    SpeciesChemicalClassAttrKey,
    SpeciesIdentifierAttrKey,
    SpeciesPropertyAttrKey,
    SpeciesUseAttrKey,
)
from .link_entity import SpeciesLinker, get_species_linker

logger = logging.getLogger(__name__)


class OntoSpeciesAgent:
    def __init__(self, kg_client: KgClient, species_linker: SpeciesLinker):
        self.kg_client = kg_client
        self.species_linker = species_linker

    def lookup_iri_attribute(
        self, species_iri: str, attr_key: SpeciesAttrKey
    ) -> List[Dict[str, str]]:
        if isinstance(attr_key, SpeciesUseAttrKey):
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Use WHERE {{
    OPTIONAL {{
        <{IRI}> os:hasUse/rdfs:label ?Use .
    }}
}}"""
        elif isinstance(attr_key, SpeciesChemicalClassAttrKey):
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?ChemicalClass WHERE {{
    OPTIONAL {{
        <{IRI}> (a|!a)+ [ a os:ChemicalClass ; rdfs:label ?ChemicalClass ] .
    }}
}}"""
        elif isinstance(attr_key, SpeciesIdentifierAttrKey):
            template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?{key} WHERE {{{{
    OPTIONAL {{
        <{{IRI}}> os:has{key}/os:value ?{key} .
    }}
}}}}""".format(
                key=attr_key.value
            )
        else:
            template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Value ?Unit ?ReferenceStateValue ?ReferenceStateUnit WHERE {{{{
    OPTIONAL {{{{
        <{{IRI}}> os:has{key} ?{key} .
        ?{key} os:value ?Value ; os:unit/rdfs:label ?Unit .
        OPTIONAL {{{{
            ?{key} os:hasReferenceState [ os:value ?ReferenceStateValue ; os:unit/rdfs:label ?ReferenceStateUnit ] .
        }}}}
    }}}}
}}}}""".format(
                key=attr_key.value
            )
        query = template.format(IRI=species_iri)
        return [
            {k: v["value"] for k, v in x.items()}
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    def lookup_chemicalSpecies_attributes(
        self, species: str, attr_keys: List[SpeciesAttrKey]
    ):
        species_iris = self.species_linker.link(species)

        bindings: List[dict] = []
        for iri in species_iris:
            datum = dict(IRI=iri)
            for key in attr_keys:
                datum[key.value] = self.lookup_iri_attribute(iri, key)
            bindings.append(datum)

        return bindings

    def find_chemicalSpecies(
        self,
        chemical_classes: List[str] = [],
        uses: List[str] = [],
        properties: List[
            Tuple[SpeciesPropertyAttrKey, CompoundNumericalConstraint]
        ] = [],
    ) -> List[str]:
        patterns = []

        for chemical_class in chemical_classes:
            patterns.append(
                '?Species (a|!a)+ [ a os:ChemicalClass ; rdfs:label "{label}" ] .'.format(
                    label=chemical_class
                )
            )

        for use in uses:
            patterns.append(
                '?Species os:hasUse/rdfs:label "{label}" .'.format(label=use)
            )

        for key, compound_constraint in properties:
            patterns.append(
                "?Species os:has{key}/os:value ?{key}Value .".format(key=key.value)
            )
            atomic_constraints = [
                "?{key}Value {operator} {operand}".format(
                    key=key.value, operator=x.operator.value, operand=x.operand
                )
                for x in compound_constraint.constraints
            ]
            if compound_constraint.logical_operator:
                delimiter = compound_constraint.logical_operator.value
            else:
                delimiter = "&&"
            exprn = delimiter.join(atomic_constraints)
            patterns.append("FILTER ( {exprn} )".format(exprn=exprn))

        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Species WHERE {{
{patterns}
}}""".format(
            patterns="\n".join(patterns)
        )

        logger.info("SPARQL query: " + query)

        return [
            x["Species"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]


def get_ontospecies_agent(
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
    species_linker: Annotated[SpeciesLinker, Depends(get_species_linker)],
):
    return OntoSpeciesAgent(kg_client=kg_client, species_linker=species_linker)
