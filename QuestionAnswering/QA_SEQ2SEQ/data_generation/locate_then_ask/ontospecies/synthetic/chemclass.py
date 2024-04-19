import json
import os
import random
from typing import Optional

from constants.fs import ROOTDIR


class OSChemClassSynthesizer:
    CHEMCLASSES_FILEPATH = "data/ontospecies/ChemicalClasses.json"
    CHEMCLASS_NUM = 3

    def __init__(self, kg_endpoint: Optional[str] = None):
        abs_filepath = os.path.join(ROOTDIR, self.CHEMCLASSES_FILEPATH)
        if not os.path.exists(abs_filepath):
            if kg_endpoint is None:
                raise ValueError(
                    "No cache of chemclasses found, `kg_endpoint` must be provided."
                )

            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(kg_endpoint)

            chemclasses = set()
            query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Label WHERE {{
    ?x a os:ChemicalClass ; rdfs:label ?Label .
}}
ORDER BY RAND()
LIMIT 1000"""
            bindings = kg_client.query(query)["results"]["bindings"]
            chemclasses.update([binding["Label"]["value"] for binding in bindings])

            chemclasses = list(chemclasses)
            chemclasses.sort()
            os.makedirs(os.path.dirname(abs_filepath), exist_ok=True)
            with open(abs_filepath, "w") as f:
                json.dump(chemclasses, f, indent=4)
        else:
            with open(abs_filepath, "r") as f:
                chemclasses = json.load(f)

        self.chemclasses = chemclasses

    def make(self):
        return random.sample(
            self.chemclasses, min(self.CHEMCLASS_NUM, len(self.chemclasses))
        )
