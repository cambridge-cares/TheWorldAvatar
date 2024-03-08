import logging
from typing import Annotated, Dict, List, Tuple

from fastapi import Depends

from model.constraint import CompoundNumericalConstraint
from services.kg_client import KgClient
from services.retrieve_docs import DocsRetriever, get_docs_retriever
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
        docs_retriever: DocsRetriever,
        cosine_similarity_threshold: float = 0.5,
    ):
        self.kg_client = kg_client
        self.species_linker = species_linker
        self.docs_retriever = docs_retriever
        self.cosine_similarity_threshold = cosine_similarity_threshold

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

    def _get_chemical_classes(self):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:ChemicalClass ; rdfs:label ?Label .
}"""
        return [
            x["Label"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    def _get_uses(self):
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {
    ?s a os:Use ; rdfs:label ?Label .
}"""
        return [
            x["Label"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    def _filter(self, docs_scores: List[Tuple[str, float]]):
        """Filters for docs below cosine similarity threshold.
        If all docs are below the threshold, return the first doc."""
        docs = [
            doc
            for doc, score in docs_scores
            if score < self.cosine_similarity_threshold
        ]
        if not docs:
            docs = [docs_scores[0][0]]
        return docs

    def _align_chemical_classes(self, chemical_classes: List[str]):
        docs_scores_lst = self.docs_retriever.retrieve(
            queries=chemical_classes,
            key="ontospecies:chemical_classes",
            docs_getter=self._get_chemical_classes,
        )
        return [self._filter(docs_scores) for docs_scores in docs_scores_lst]

    def _align_uses(self, uses: List[str]):
        docs_scores_lst = self.docs_retriever.retrieve(
            queries=uses,
            key="ontospecies:uses",
            docs_getter=self._get_uses,
        )
        return [self._filter(docs_scores) for docs_scores in docs_scores_lst]

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
            aligned_chemical_classes = self._align_chemical_classes(chemical_classes)
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
            aligned_uses = self._align_uses(uses)
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
    docs_retriever: Annotated[DocsRetriever, Depends(get_docs_retriever)],
):
    return OntoSpeciesAgent(
        kg_client=kg_client,
        species_linker=species_linker,
        docs_retriever=docs_retriever,
    )
