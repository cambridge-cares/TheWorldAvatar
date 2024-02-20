from dataclasses import dataclass
from decimal import Decimal
from typing import Dict

from constants.ontozeolite import OZZeoFwAttrKey


@dataclass(frozen=True)
class OZZeoFw:
    iri: str
    framework_code: str  # zeo:hasFrameworkCode
    unit_cell_volume: Decimal  # ocr:hasCrystalInformation/ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue
    tile_code: str  # ocr:hasCrystalInformation/ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode
    topology_measures: Dict[OZZeoFwAttrKey, Decimal]  # zeo:hasZeoliteTopology/zeo:has{key}/om:hasNumericalValue
