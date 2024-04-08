from collections import defaultdict
from dataclasses import asdict, dataclass
from decimal import Decimal
import json
import os
import random
from typing import DefaultDict, Dict, List, Optional

from constants.fs import ROOTDIR
from constants.ontozeolite import (
    ZEOTOPO_SCALAR_KEYS,
    OZZeoTopoAttrKey,
)
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZCrystalInfo, OZFramework
from utils.json import EnumEncoder, as_enum
from .crystal_info import retrieve_seed_crystalInfo


@dataclass
class OZFrameworkSeed:
    framework_codes: List[str]
    crystal_scalars: List[OZCrystalInfo]
    topo_scalars: Dict[OZZeoTopoAttrKey, List[Decimal]]
    material_counts: List[int]
    framework_components: List[List[str]]
    guest_species_counts: List[int]


class OZFrameworkSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data", "ontozeolite", "Framework.json")

    @classmethod
    def _retrieve_seed_frameworkCodes(cls, kg_client: KgClient):
        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT ?FrameworkCode WHERE {{
  ?Framework zeo:hasFrameworkCode ?FrameworkCode
}}
LIMIT 100"""
        return [
            x["FrameworkCode"]["value"]
            for x in kg_client.query(query)["results"]["bindings"]
        ]

    @classmethod
    def _retrieve_seed_topoScalars(cls, kg_client: KgClient):
        def helper(key: OZZeoTopoAttrKey):
            query = """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT DISTINCT ?o WHERE {{
    ?Framework zeo:hasTopologicalProperties/zeo:has{key}/om:hasNumericalValue ?o
}}
LIMIT 100""".format(
                key=key.value
            )
            return [
                Decimal(binding["o"]["value"])
                for binding in kg_client.query(query)["results"]["bindings"]
            ]

        return {key: helper(key) for key in ZEOTOPO_SCALAR_KEYS}

    @classmethod
    def _retrieve_seed_materialCounts(cls, kg_client: KgClient):
        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT (COUNT(?Material) AS ?Count) WHERE {
    ?Framework zeo:hasZeoliticMaterial ?Material .
}
GROUP BY ?Framework
LIMIT 100"""
        return [
            int(binding["Count"]["value"])
            for binding in kg_client.query(query)["results"]["bindings"]
        ]

    @classmethod
    def _retrieve_seed_frameworkComponents(
        cls, kg_client: KgClient, ontospecies_endpoint: str
    ):
        query = """PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
        
SELECT DISTINCT ?Framework (SAMPLE(?ElementLabel) AS ?ElementLabelSample) WHERE {{
    ?Framework zeo:hasZeoliticMaterial/zeo:hasFrameworkComponent ?Element .
    SERVICE <{ontospecies_endpoint}> {{
        ?Element (os:hasElementName|os:hasElementSymbol)/os:value ?ElementLabel
    }}
}}
GROUP BY ?Framework ?Element
LIMIT 100""".format(
            ontospecies_endpoint=ontospecies_endpoint
        )
        framework2elements: DefaultDict[str, List[str]] = defaultdict(list)
        for binding in kg_client.query(query)["results"]["bindings"]:
            framework2elements[binding["Framework"]["value"]].append(
                binding["ElementLabelSample"]["value"]
            )

        return list(framework2elements.values())

    @classmethod
    def _retrieve_seed_guestSpeciesCounts(cls, kg_client: KgClient):
        query = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT (COUNT(?Guest) AS ?Count) WHERE {
    ?Framework zeo:hasZeoliticMaterial/zeo:hasGuestCompound ?Guest .
}
GROUP BY ?Framework
LIMIT 100"""
        return [
            int(binding["Count"]["value"])
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
                    "No cache for zeolite framework data found, `kg_endpoint` must not be None."
                )

            if ontospecies_endpoint is None:
                raise ValueError(
                    "No cache for zeolite framework daa found, `ontospecies_endpoint` must not be None."
                )

            kg_client = KgClient(kg_endpoint, user, pw)
            seed_data = OZFrameworkSeed(
                framework_codes=self._retrieve_seed_frameworkCodes(kg_client),
                crystal_scalars=retrieve_seed_crystalInfo(kg_client, "Framework"),
                topo_scalars=self._retrieve_seed_topoScalars(kg_client),
                material_counts=self._retrieve_seed_materialCounts(kg_client),
                framework_components=self._retrieve_seed_frameworkComponents(
                    kg_client, ontospecies_endpoint
                ),
                guest_species_counts=self._retrieve_seed_guestSpeciesCounts(kg_client),
            )

            os.makedirs(os.path.dirname(self.FILEPATH), exist_ok=True)
            with open(self.FILEPATH, "w") as f:
                json.dump(asdict(seed_data), f, indent=4,  cls=EnumEncoder)
        else:
            with open(self.FILEPATH, "r") as f:
                seed_data = OZFrameworkSeed(**json.load(f, object_hook=as_enum))

        self.seed_data = seed_data

    def make(self):
        return OZFramework(
            iri="placeholder",
            framework_code=random.choice(self.seed_data.framework_codes),
            crystal_info=random.choice(self.seed_data.crystal_scalars),
            topo_scalar={
                k: random.choice(v) for k, v in self.seed_data.topo_scalars.items()
            },
            material_iris=[
                "placeholder"
                for _ in range(random.choice(self.seed_data.material_counts))
            ],
            framework_components=random.choice(self.seed_data.framework_components),
            guest_species_iris=[
                "placeholder"
                for _ in range(random.choice(self.seed_data.guest_species_counts))
            ],
        )
