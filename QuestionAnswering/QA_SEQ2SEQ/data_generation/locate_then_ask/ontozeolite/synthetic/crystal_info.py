from decimal import Decimal
import json
import os
import random

from typing import Optional
from constants.fs import ROOTDIR
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZCrystalInfo


class OZCrystalInfoSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data", "ontozeolite", "CrystalInfo.json")

    def __init__(self, kg_endpoint: Optional[str] = None):
        if not os.path.exists(self.FILEPATH):
            if kg_endpoint is None:
                raise ValueError(
                    "No crystal info cache found, `kg_endpoint` must be set."
                )

            kg_client = KgClient(kg_endpoint)

            template = """PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT DISTINCT ?o WHERE {{
  ?s {pred} ?o
}}
LIMIT 100"""

            data = dict()
            for key, pred in [
                ("UnitCellVolume", "ocr:hasUnitCellVolume/om:hasNumericalValue"),
                ("TileCode", "ocr:hasTileCode"),
            ]:
                query = template.format(pred=pred)
                data[key] = [x["o"]["value"] for x in kg_client.query(query)["results"]["bindings"]]

            os.makedirs(os.path.dirname(self.FILEPATH), exist_ok=True)
            with open(self.FILEPATH, "w") as f:
                json.dump(data, f, indent=4)
        else:
            with open(self.FILEPATH, "r") as f:
                data = json.load(f)

        self.data = data

    def make(self):
        return OZCrystalInfo(
            unit_cell_volume=Decimal(random.choice(self.data["UnitCellVolume"])),
            tile_code=random.choice(self.data["TileCode"]),
        )
