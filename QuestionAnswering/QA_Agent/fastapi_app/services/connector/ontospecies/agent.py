import logging
from typing import Annotated, Dict, List, Tuple

from fastapi import Depends

from model.constraint import CompoundNumericalConstraint
from services.kg_client import KgClient
from .align import OntoSpeciesAligner, get_ontospecies_aligner
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
    def __init__(
        self,
        kg_client: KgClient,
        species_linker: SpeciesLinker,
        ontospecies_aligner: OntoSpeciesAligner,
    ):
        self.kg_client = kg_client
        self.species_linker = species_linker
        self.aligner = ontospecies_aligner

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

    def lookup_iri_label(self, species_iri: str):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        
SELECT ?Label WHERE {{
    <{IRI}> rdfs:label ?Label .
}}""".format(
            IRI=species_iri
        )
        return self.kg_client.query(query)["results"]["bindings"][0]["Label"]["value"]

    def find_chemicalSpecies(
        self,
        chemical_classes: List[str] = [],
        uses: List[str] = [],
        properties: List[
            Tuple[SpeciesPropertyAttrKey, CompoundNumericalConstraint]
        ] = [],
    ) -> List[str]:
        patterns = []

        if chemical_classes:
            logger.info("Aligning chemical class labels...")
            aligned_chemical_classes = self.aligner.align_chemical_classes(
                chemical_classes
            )
            logger.info("Aligned chemical classes: " + str(aligned_chemical_classes))

            for i, chemclass_options in enumerate(aligned_chemical_classes):
                varnode = "?ChemicalClassLabel" + str(i)
                values = ['"{val}"'.format(val=val) for val in chemclass_options]
                patterns.extend(
                    [
                        "VALUES {varnode} {{ {values} }}".format(
                            varnode=varnode, values=" ".join(values)
                        ),
                        "?Species (a|!a)+ [ a os:ChemicalClass ; rdfs:label {varnode} ] .".format(
                            varnode=varnode
                        ),
                    ]
                )

        if uses:
            logger.info("Aligning use labels...")
            aligned_uses = self.aligner.align_uses(uses)
            logger.info("Aligned uses: " + str(aligned_uses))

            for i, use_options in enumerate(aligned_uses):
                varnode = "?UseLabel" + str(i)
                values = ['"{val}"'.format(val=val) for val in use_options]
                patterns.extend(
                    [
                        "VALUES {varnode} {{ {values} }}".format(
                            varnode=varnode, values=" ".join(values)
                        ),
                        "?Species os:hasUse/rdfs:label {varnode} .".format(
                            varnode=varnode
                        ),
                    ]
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

        iris = [
            x["Species"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]
        labels = [self.lookup_iri_label(iri) for iri in iris]
        return [dict(IRI=iri, label=label) for iri, label in zip(iris, labels)]


def get_ontospecies_agent(
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
    species_linker: Annotated[SpeciesLinker, Depends(get_species_linker)],
    ontospecies_aligner: Annotated[
        OntoSpeciesAligner, Depends(get_ontospecies_aligner)
    ],
):
    return OntoSpeciesAgent(
        kg_client=kg_client,
        species_linker=species_linker,
        ontospecies_aligner=ontospecies_aligner,
    )
