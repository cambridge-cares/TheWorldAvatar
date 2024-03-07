import logging
from typing import Dict, List

from .constants import (
    SpeciesAttrKey,
    SpeciesChemicalClassAttrKey,
    SpeciesIdentifierAttrKey,
    SpeciesUseAttrKey,
)
from .kg_client import get_ontospecies_kg_client

logger = logging.getLogger(__name__)


class SpeciesAttributeRetriever:
    def __init__(self):
        self.kg_client = get_ontospecies_kg_client()

    def retrieve(
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
