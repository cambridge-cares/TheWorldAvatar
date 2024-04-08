from dataclasses import dataclass
from decimal import Decimal

from typing import Literal
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontozeolite.model import OZCrystalInfo


def retrieve_seed_crystalInfo(
    kg_client: KgClient, rdf_type: Literal["Framework", "Material"]
):
    if rdf_type == "Framework":
        query = """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT ?UnitCellVolume ?TileCode WHERE {
    ?s a zeo:ZeoliteFramework . 
    ?s ocr:hasCrystalInformation [
        ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue ?UnitCellVolume ;
        ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode ?TileCode
    ] .
}
LIMIT 100"""
    else:
        query == """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>

SELECT ?UnitCellVolume ?TileCode WHERE {
    ?s a zeo:ZeoliticMaterial . 
    ?s ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue ?UnitCellVolume .
    ?s ^zeo:hasZeoliticMaterial/ocr:hasCrystalInformation/ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode ?TileCode .
}
LIMIT 100"""

    return [
        OZCrystalInfo(
            unit_cell_volume=Decimal(binding["UnitCellVolume"]["value"]),
            tile_code=binding["TileCode"]["value"],
        )
        for binding in kg_client.query(query)["results"]["bindings"]
    ]
