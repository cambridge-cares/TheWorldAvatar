from dataclasses import dataclass
from decimal import Decimal
from typing import Dict, Optional, Tuple

from constants.ontozeolite import OZZeoTopoAttrKey


@dataclass(frozen=True)
class OZCrystalInfo:
    unit_cell_volume: (
        Decimal  # ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue
    )
    tile_code: str  # ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode


@dataclass(frozen=True)
class OZFramework:
    iri: str
    framework_code: str  # zeo:hasFrameworkCode
    crystal_info: OZCrystalInfo  # ocr:hasCrystalInformation
    topo_scalar: Dict[
        OZZeoTopoAttrKey, Decimal
    ]  # zeo:hasZeoliteTopology/zeo:has{key}/om:hasNumericalValue


@dataclass(frozen=True)
class OZMaterial:
    iri: str
    framework_iri: str  # ^zeo:hasZeoliticMaterial
    formulae: Tuple[str, ...]  # zeo:hasChemicalFormula
    guest_compound: Optional[str]  # zeo:hasGuestCompound/os:formula

    def __post_init__(self):
        object.__setattr__(self, "formulae", tuple(self.formulae))
