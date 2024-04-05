from decimal import Decimal
from typing import Dict, List, Optional, Tuple

from pydantic.dataclasses import dataclass

from constants.ontozeolite import OZZeoTopoAttrKey


@dataclass(frozen=True)
class OZCrystalInfo:
    unit_cell_volume: (
        Decimal  # ocr:hasUnitCell/ocr:hasUnitCellVolume/om:hasNumericalValue
    )
    tile_code: Optional[str]  # ocr:hasTiledStructure/ocr:hasTile/ocr:hasTileCode


@dataclass(frozen=True)
class OZFramework:
    iri: str
    framework_code: str  # zeo:hasFrameworkCode
    framework_components: List[
        str
    ]  # zeo:hasFrameworkComponent/os:hasElementSymbol/os:value
    crystal_info: OZCrystalInfo  # ocr:hasCrystalInformation
    topo_scalar: Dict[
        OZZeoTopoAttrKey, Decimal
    ]  # zeo:hasZeoliticProperties/zeo:has{key}/om:hasNumericalValue
    material_iris: List[str]  # zeo:hasZeoliticMaterial
    guest_species_iris: List[str]  # zeo:hasZeoliticMaterial/zeo:hasGuestCompound
    guest_formulae: List[str]  # zeo:hasZeoliticMaterial/zeo:hasGuestFormula


@dataclass(frozen=True)
class OZMaterial:
    iri: str
    framework_iri: str  # ^zeo:hasZeoliticMaterial
    formulae: Tuple[str, ...]  # zeo:hasChemicalFormula
    guest_species_iris: List[str]  # zeo:hasGuestCompound
    guest_formula: Optional[str]

    def __post_init__(self):
        object.__setattr__(self, "formulae", tuple(self.formulae))
