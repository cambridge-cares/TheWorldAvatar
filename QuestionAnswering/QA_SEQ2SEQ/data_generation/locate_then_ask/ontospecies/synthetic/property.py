from decimal import Decimal
import json
import os
import random
from typing import Dict, Tuple

from constants.fs import ROOTDIR
from constants.ontospecies import OSPropertyKey
from locate_then_ask.ontospecies.model import OSProperty


class OSPropertySynthesizer:
    PROPERTY_DATAPOINTS_FILEPATH = "data/ontospecies/PropertyDataPoints.json"
    INT_PROPERTIES = [
        OSPropertyKey.ATOM_CHIRAL_COUNT,
        OSPropertyKey.ATOM_CHIRAL_DEF_COUNT,
        OSPropertyKey.ATOM_CHIRAL_UNDEF_COUNT,
        OSPropertyKey.BOND_CHIRAL_COUNT,
        OSPropertyKey.BOND_CHIRAL_DEF_COUNT,
        OSPropertyKey.BOND_CHIRAL_UNDEF_COUNT,
        OSPropertyKey.CHARGE,
        OSPropertyKey.COVALENT_UNIT_COUNT,
        OSPropertyKey.HEAVY_ATOM_COUNT,
        OSPropertyKey.HYDROGEN_BOND_ACCEPTOR_COUNT,
        OSPropertyKey.HYDROGEN_BOND_DONOR_COUNT,
        OSPropertyKey.ISOTOPE_ATOM_COUNT,
        OSPropertyKey.ROTATABLE_BOND_COUNT,
        OSPropertyKey.TAUTOMERS_COUNT,
    ]

    def __init__(self):
        abs_filepath = os.path.join(ROOTDIR, self.PROPERTY_DATAPOINTS_FILEPATH)
        if not os.path.exists(abs_filepath):
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(
                "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql"
            )

            query_template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT DISTINCT ?Value WHERE {{
    ?x a os:{Prop} ; os:value ?Value .
}}
ORDER BY RAND()
LIMIT 100"""
            prop2data: Dict[str, Tuple[float, float]] = dict()
            for prop in OSPropertyKey:
                query = query_template.format(Prop=prop.value)
                bindings = kg_client.query(query)["results"]["bindings"]
                prop2data[prop.value] = [float(x["Value"]["value"]) for x in bindings]

            os.makedirs(os.path.dirname(abs_filepath), exist_ok=True)
            with open(abs_filepath, "w") as f:
                json.dump(prop2data, f, indent=4)
        else:
            with open(abs_filepath, "r") as f:
                prop2data = json.load(f)

        self.prop2data = {OSPropertyKey(key): value for key, value in prop2data.items()}

    def _resolve(self, key: OSPropertyKey, value: float):
        if key in self.INT_PROPERTIES:
            value = int(value)
        return Decimal(str(value))

    def make(self):
        return {
            key: [
                OSProperty(self._resolve(key, value), unit="placeholder_unit")
                for value in random.sample(data, k=min(3, len(data)))
            ]
            for key, data in self.prop2data.items()
        }
