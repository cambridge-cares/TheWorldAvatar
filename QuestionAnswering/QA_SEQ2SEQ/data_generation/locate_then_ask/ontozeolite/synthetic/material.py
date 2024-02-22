import json
import os
import random
from typing import Optional

from constants.fs import ROOTDIR
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZMaterial
from .crystal_info import OZCrystalInfoSynthesizer


class OZMaterialSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data", "ontozeolite", "Material.json")

    def __init__(self, kg_endpoint: Optional[str] = None):
        self.crystalinfo_synth = OZCrystalInfoSynthesizer(kg_endpoint)

        if not os.path.exists(self.FILEPATH):
            if kg_endpoint is None:
                raise ValueError(
                    "No cache for zeolite material data found, `kg_endpoint` must not be None."
                )

            kg_client = KgClient(kg_endpoint)

            template = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT ?o WHERE {{
  ?s {pred} ?o
}}
LIMIT 100"""
            data = dict()
            for key, pred in [
                ("ChemicalFormula", "zeo:hasChemicalFormula"),
                ("GuestCompound", "zeo:hasGuestCompound/os:formula"),
            ]:
                query = template.format(pred=pred)
                data[key] = [
                    x["o"]["value"]
                    for x in kg_client.query(query)["results"]["bindings"]
                ]

            os.makedirs(os.path.dirname(self.FILEPATH), exist_ok=True)
            with open(self.FILEPATH, "w") as f:
                json.dump(data, f, indent=4)
        else:
            with open(self.FILEPATH, "r") as f:
                data = json.load(f)

        self.data = data

    def make(self):
        return OZMaterial(
            iri="placeholder",
            framework_iri="placeholder",
            formulae=[random.choice(self.data["ChemicalFormula"])],
            guest_compound=random.choice(self.data["GuestCompound"]),
        )
