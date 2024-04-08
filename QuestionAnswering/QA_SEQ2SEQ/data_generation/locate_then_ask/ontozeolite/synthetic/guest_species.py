from collections import defaultdict
import json
import os
import random
from typing import Optional

from constants.fs import ROOTDIR
from locate_then_ask.kg_client import KgClient


class OZGuestSpeciesSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data", "ontozeolite", "GuestSpecies.json")

    def __init__(
        self,
        kg_endpoint: Optional[str] = None,
        user: Optional[str] = None,
        pw: Optional[str] = None,
        ontospecies_endpoint: Optional[str] = None,
    ):
        if not os.path.exists(self.FILEPATH):
            if kg_endpoint is None:
                raise ValueError(
                    "No cache for guest species data found, `kg_endpoint` must not be None."
                )
            if ontospecies_endpoint is None:
                raise ValueError(
                    "No cache for guest species data found, `ontospecies_endpoint` must not be None."
                )

            kg_client = KgClient(kg_endpoint, user, pw)

            query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT ?Species ?SpeciesLabel WHERE {{
    ?s zeo:hasGuestCompound ?Species .
    SERVICE <{ontospecies_endpoint}> {{
        VALUES ?hasIdentifier {{ os:hasInChI os:hasIUPACName os:hasMolecularFormula os:hasSMILES }}
        ?Species ?hasIdentifier [ os:value ?SpeciesLabel ] .
    }}
}}
LIMIT 500""".format(
                ontospecies_endpoint=ontospecies_endpoint
            )
            iri2labels = defaultdict(list)
            for binding in kg_client.query(query)["results"]["bindings"]:
                iri2labels[binding["Species"]["value"]].append(
                    binding["SpeciesLabel"]["value"]
                )

            labels = list(iri2labels.values())
            with open(self.FILEPATH, "w") as f:
                json.dump(labels, f)

        else:
            with open(self.FILEPATH, "r") as f:
                labels = json.load(f)

        self.labels = labels

    def get_labels(self):
        return random.choice(self.labels)
