from decimal import Decimal
from typing import Dict
from constants.ontozeolite import ZEOTOPO_SCALAR_KEYS

from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZCrystalInfo, OZFramework, OZMaterial


class OZEntityStore:
    def __init__(self, kg_endpoint: str):
        self.kg_client = KgClient(kg_endpoint)
        self.iri2framework: Dict[str, OZFramework] = dict()
        self.iri2material: Dict[str, OZMaterial] = dict()

    def get_framework(self, entity_iri: str):
        if entity_iri not in self.iri2framework:
            self.iri2framework[entity_iri] = OZFramework(
                iri=entity_iri,
                framework_code=self._retrieve_by_property(
                    entity_iri, "zeo:hasFrameworkCode"
                )[0],
                crystal_info=self._retrieve_crystal_info(entity_iri),
                topo_scalar=self._retrieve_topomeasures(entity_iri),
            )
        return self.iri2framework[entity_iri]

    def get_material(self, entity_iri: str):
        if entity_iri not in self.iri2material:
            guest_compound_values = self._retrieve_by_property(
                entity_iri, "zeo:hasGuestCompound/os:formula"
            )
            if len(guest_compound_values) == 0:
                guest_compound = None
            else:
                guest_compound = guest_compound_values[0]
            self.iri2material[entity_iri] = OZMaterial(
                iri=entity_iri,
                framework_iri=self._retrieve_by_property(
                    entity_iri, "^zeo:hasZeoliticMaterial"
                )[0],
                formulae=self._retrieve_by_property(
                    entity_iri, "zeo:hasChemicalFormula"
                ),
                guest_compound=guest_compound,
            )
        return self.iri2material[entity_iri]

    def _retrieve_by_property(self, entity_iri: str, prop: str):
        template = """PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT ?x WHERE {{
    <{IRI}> {prop} ?x .
}}
"""
        query = template.format(IRI=entity_iri, prop=prop)
        return [
            row["x"]["value"]
            for row in self.kg_client.query(query)["results"]["bindings"]
        ]

    def _retrieve_crystal_info(self, entity_iri: str):
        unit_cell_volume = Decimal(
            self._retrieve_by_property(
                entity_iri,
                "ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue",
            )[0]
        )
        tile_code_values = self._retrieve_by_property(
            entity_iri,
            "ocr:hasCrystalInformation/ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode",
        )
        if len(tile_code_values) == 0:
            tile_code = None
        else:
            tile_code = tile_code_values[0]
        return OZCrystalInfo(
            unit_cell_volume=unit_cell_volume,
            tile_code=tile_code,
        )

    def _retrieve_topomeasures(self, entity_iri: str):
        key2bindings = {
            key: self._retrieve_by_property(
                entity_iri,
                "zeo:hasZeoliteTopology/zeo:has{key}/om:hasNumericalValue".format(
                    key=key.value
                ),
            )
            for key in ZEOTOPO_SCALAR_KEYS
        }
        return {k: Decimal(v[0]) for k, v in key2bindings.items() if v}
