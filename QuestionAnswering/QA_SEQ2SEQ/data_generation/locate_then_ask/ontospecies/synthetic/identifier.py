import json
import os
import random

from constants.fs import ROOTDIR
from constants.ontospecies import OSIdentifierKey


class OSIdentifierSynthesizer:
    IDENTIFIERS_FILEPATH = "data/ontospecies/Identifiers.json"
    IDENTIFIER_NUM = 3

    def __init__(self):
        abs_filepath = os.path.join(ROOTDIR, self.IDENTIFIERS_FILEPATH)
        if not os.path.exists(abs_filepath):
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(
                "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql"
            )

            query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Value WHERE {{
    ?x a os:{Ident} ; os:value ?Value .
    {filter}
}}
ORDER BY RAND()
LIMIT 100"""

            identifier2values = dict()
            for key in OSIdentifierKey:
                query = query_template.format(
                    Ident=key.value,
                    filter=""
                    if key is not OSIdentifierKey.INCHI
                    else "FILTER ( STRLEN(STR(?Value)) < 60 ) FILTER ( STRLEN(STR(?Value)) > 10 )",
                )
                bindings = kg_client.query(query)["results"]["bindings"]
                identifier2values[key.value] = [
                    binding["Value"]["value"] for binding in bindings
                ]

            os.makedirs(os.path.dirname(abs_filepath), exist_ok=True)
            with open(abs_filepath, "w") as f:
                json.dump(identifier2values, f, indent=4)
        else:
            with open(abs_filepath, "r") as f:
                identifier2values = json.load(f)

        self.identifier2values = {
            OSIdentifierKey(key): value for key, value in identifier2values.items()
        }

    def make(self):
        return {
            key: random.sample(value, min(self.IDENTIFIER_NUM, len(value)))
            for key, value in self.identifier2values.items()
        }
