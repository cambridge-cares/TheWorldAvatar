from functools import cache
import json
from typing import Annotated, List
from fastapi import Depends

from redis import Redis

from services.kg_client import KgClient
from services.redis_client import get_redis_client
from ..kg_client import get_ontospecies_kg_client


class SpeciesLinker:
    INVERTED_INDEX_KEYNAME = "ontospecies:inverted.index"

    def __init__(self, kg_client: KgClient, redis_client: Redis):
        self.kg_client = kg_client
        self.redis_client = redis_client

    def _link(self, species: str) -> List[str]:
        query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Species WHERE {{
    ?Species a os:Species .
    VALUES ?Label {{ "{label}" }}
    {{
        ?Species rdfs:label ?Label
    }} UNION {{
        ?Species skos:altLabel ?Label
    }} UNION {{
        ?Species ?hasIdentifier [ a/rdfs:subClassOf os:Identifier ; os:value ?Label ]
    }} UNION {{
        ?Species (a|!a)+ [ a os:ChemicalClass ; rdfs:label ?Label ]
    }}
}}""".format(
            label=species
        )
        return [
            x["Species"]["value"]
            for x in self.kg_client.query(query)["results"]["bindings"]
        ]

    def link(self, species: str) -> List[str]:
        # TODO: use fuzzy matching
        if not self.redis_client.hexists(self.INVERTED_INDEX_KEYNAME, species):
            iris = self._link(species)
            self.redis_client.hset(
                self.INVERTED_INDEX_KEYNAME, species, json.dumps(iris)
            )
            return iris

        iris = self.redis_client.hget(self.INVERTED_INDEX_KEYNAME, species)
        return json.loads(iris)


@cache
def get_species_linker(
    kg_client: Annotated[KgClient, Depends(get_ontospecies_kg_client)],
    redis_client: Annotated[Redis, Depends(get_redis_client)],
):
    return SpeciesLinker(kg_client, redis_client)
