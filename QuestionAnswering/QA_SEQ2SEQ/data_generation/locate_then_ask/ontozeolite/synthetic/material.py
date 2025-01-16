from dataclasses import asdict, dataclass
import json
import os
import random
from typing import List, Optional

from constants.fs import ROOTDIR
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZCrystalInfo, OZMaterial
from locate_then_ask.ontozeolite.synthetic.helpers import (
    retrieve_seed_crystalInfo,
    retrieve_seed_frameworkComponents,
    retrieve_seed_guestSpeciesCounts,
)
from utils.json import EnumEncoder, as_enum


@dataclass
class OZMaterialSeed:
    formulae: List[str]
    crystal_scalars: List[OZCrystalInfo]
    framework_components: List[List[str]]
    guest_species_counts: List[int]


class OZMaterialSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data", "ontozeolite", "Material.json")

    @classmethod
    def _retrieve_seed_formulae(cls, kg_client: KgClient):
        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT ?Formula WHERE {
    ?Material a zeo:ZeoliticMaterial .
    ?Material zeo:hasChemicalFormula ?Formula .
}
LIMIT 100"""
        return [
            binding["Formula"]["value"]
            for binding in kg_client.query(query)["results"]["bindings"]
        ]

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
                    "No cache for zeolitic material data found, `kg_endpoint` must not be None."
                )
            if ontospecies_endpoint is None:
                raise ValueError(
                    "No cache for zeolitic material data found, `ontospecies_endpoint` must not be None."
                )

            kg_client = KgClient(kg_endpoint, user, pw)
            seed_data = OZMaterialSeed(
                formulae=self._retrieve_seed_formulae(kg_client),
                crystal_scalars=retrieve_seed_crystalInfo(
                    kg_client, clsname="Material"
                ),
                framework_components=retrieve_seed_frameworkComponents(
                    kg_client, ontospecies_endpoint, clsname="Material"
                ),
                guest_species_counts=retrieve_seed_guestSpeciesCounts(
                    kg_client, clsname="Material"
                ),
            )

            os.makedirs(os.path.dirname(self.FILEPATH), exist_ok=True)
            with open(self.FILEPATH, "w") as f:
                json.dump(asdict(seed_data), f, indent=4, cls=EnumEncoder)
        else:
            with open(self.FILEPATH, "r") as f:
                seed_data = OZMaterialSeed(**json.load(f, object_hook=as_enum))

        self.seed_data = seed_data

    def make(self):
        return OZMaterial(
            iri="placeholder",
            formula=random.choice(self.seed_data.formulae),
            framework_iri="placeholder",
            crystal_info=random.choice(self.seed_data.crystal_scalars),
            framework_components=random.choice(self.seed_data.framework_components),
            guest_species_iris=[
                "placeholder"
                for _ in range(random.choice(self.seed_data.guest_species_counts))
            ],
        )
