from decimal import Decimal
import json
import os
import random
from typing import Optional

from constants.fs import ROOTDIR
from constants.ontozeolite import ZEOTOPO_SCALAR_KEYS
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZFramework
from .crystal_info import OZCrystalInfoSynthesizer


class OZFrameworkSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data", "ontozeolite", "Framework.json")

    def __init__(self, kg_endpoint: Optional[str] = None):
        self.crystalinfo_synth = OZCrystalInfoSynthesizer(kg_endpoint)

        if not os.path.exists(self.FILEPATH):
            if kg_endpoint is None:
                raise ValueError(
                    "No cache for zeolite framework data found, `kg_endpoint` must not be None."
                )

            kg_client = KgClient(kg_endpoint)

            template = """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT ?o WHERE {{
  ?s {pred} ?o
}}
LIMIT 100"""
            data = dict()
            for key, pred in [("FrameworkCode", "zeo:hasFrameworkCode")] + [
                (x.value, "zeo:has{key}/om:hasNumericalValue".format(key=x.value))
                for x in ZEOTOPO_SCALAR_KEYS
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
        return OZFramework(
            iri="placeholder",
            framework_code=random.choice(self.data["FrameworkCode"]),
            crystal_info=self.crystalinfo_synth.make(),
            topo_scalar={
                k: Decimal(random.choice(self.data[k.value]))
                for k in ZEOTOPO_SCALAR_KEYS
            },
        )
