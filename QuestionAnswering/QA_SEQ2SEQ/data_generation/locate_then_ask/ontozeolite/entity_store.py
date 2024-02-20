from decimal import Decimal
from typing import Dict
from constants.ontozeolite import OZZeoFwAttrKey

from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZZeoFw


class OZEntityStore:
    TOPOLOGY_MEASURE_KEYS = [
        OZZeoFwAttrKey.ACCESSIBLE_AREA_PER_CELL,
        OZZeoFwAttrKey.ACCESSIBLE_AREA_PER_GRAM,
        OZZeoFwAttrKey.ACCESSIBLE_VOLUME,
        OZZeoFwAttrKey.ACCESSIBLE_VOLUME_PER_CELL,
        OZZeoFwAttrKey.OCCUPIABLE_AREA_PER_CELL,
        OZZeoFwAttrKey.OCCUPIABLE_AREA_PER_GRAM,
        OZZeoFwAttrKey.OCCUPIABLE_VOLUME,
        OZZeoFwAttrKey.OCCUPIABLE_VOLUME_PER_CELL,
        OZZeoFwAttrKey.SPECIFIC_ACCESSIBLE_AREA,
        OZZeoFwAttrKey.SPECIFIC_OCCUPIABLE_AREA,
        OZZeoFwAttrKey.DENSITY,
        OZZeoFwAttrKey.FRAMEWORK_DENSITY,
    ]

    def __init__(self, kg_endpoint: str):
        self.kg_client = KgClient(kg_endpoint)
        self.iri2entity: Dict[str, OZZeoFw] = dict()

    def get(self, entity_iri: str):
        if entity_iri not in self.iri2entity:
            self.iri2entity[entity_iri] = OZZeoFw(
                iri=entity_iri,
                framework_code=self._retrieve_by_property(
                    entity_iri, "zeo:hasFrameworkCode"
                )[0],
                unit_cell_volume=self._retrieve_by_property(
                    entity_iri,
                    "ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue",
                )[0],
                tile_code=self._retrieve_by_property(
                    entity_iri,
                    "ocr:hasCrystalInformation/ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode",
                )[0],
                topology_measures=self._retrieve_topomeasures(entity_iri),
            )
        return self.iri2entity[entity_iri]

    def _retrieve_by_property(self, entity_iri: str, prop: str):
        template = """PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?x WHERE {{
    <{IRI}> {prop} ?x .
}}
"""
        query = template.format(IRI=entity_iri, prop=prop)
        return [
            row["x"]["value"]
            for row in self.kg_client.query(query)["results"]["bindings"]
        ]

    def _retrieve_topomeasures(self, entity_iri: str):
        key2bindings = {
            key: self._retrieve_by_property(
                entity_iri,
                "zeo:hasZeoliteTopology/zeo:has{key}/om:hasNumericalValue".format(
                    key=key.value
                ),
            )
            for key in self.TOPOLOGY_MEASURE_KEYS
        }
        return {k: Decimal(v[0]) for k, v in key2bindings.items() if v}
