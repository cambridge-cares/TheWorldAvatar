import json
import os
import random

from constants.fs import ROOTDIR


class OSUseSynthesizer:
    USES_FILEPATH = "data/ontospecies/Uses.json"
    USE_NUM = 3

    def __init__(self):
        abs_filepath = os.path.join(ROOTDIR, self.USES_FILEPATH)
        if not os.path.exists(abs_filepath):
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(
                "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql"
            )

            uses = set()
            query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {{
    ?x a os:Use ; rdfs:label ?Label .
}}
ORDER BY RAND()
LIMIT 1000"""
            bindings = kg_client.query(query)["results"]["bindings"]
            uses.update([binding["Label"]["value"] for binding in bindings])

            uses = list(uses)
            uses.sort()
            os.makedirs(os.path.dirname(abs_filepath), exist_ok=True)
            with open(abs_filepath, "w") as f:
                json.dump(uses, f, indent=4)
        else:
            with open(abs_filepath, "r") as f:
                uses = json.load(f)

        self.uses = uses

    def make(self):
        return random.sample(
            self.uses, min(self.USE_NUM, len(self.uses))
        )
