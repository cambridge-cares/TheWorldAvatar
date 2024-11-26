from enum import Enum
from typing import Literal
from pydantic import BaseModel

from model.kg.ontozeolite import CrystalInfoKey, TopologicalPropertyKey
from model.web.comp_op import ComparisonOperator


class XRDPeakRequest(BaseModel):
    position: float
    width: float = 0.5
    threshold: float = 50


class UnitCellLengthKey(str, Enum):
    A = "a"
    B = "b"
    C = "c"


class UnitCellAngleKey(str, Enum):
    ALPHA = "alpha"
    BETA = "beta"
    GAMMA = "gamma"


UnitCellKey = UnitCellLengthKey | UnitCellAngleKey


ScalarTopologicalPropertyKey = Literal[
    TopologicalPropertyKey.ACCESSIBLE_AREA_PER_CELL,
    TopologicalPropertyKey.ACCESSIBLE_AREA_PER_GRAM,
    TopologicalPropertyKey.ACCESSIBLE_VOLUME,
    TopologicalPropertyKey.ACCESSIBLE_VOLUME_PER_CELL,
    TopologicalPropertyKey.OCCUPIABLE_AREA_PER_CELL,
    TopologicalPropertyKey.OCCUPIABLE_AREA_PER_GRAM,
    TopologicalPropertyKey.OCCUPIABLE_VOLUME,
    TopologicalPropertyKey.OCCUPIABLE_VOLUME_PER_CELL,
    TopologicalPropertyKey.SPECIFIC_ACCESSIBLE_AREA,
    TopologicalPropertyKey.SPECIFIC_OCCUPIABLE_AREA,
    TopologicalPropertyKey.DENSITY,
    TopologicalPropertyKey.FRAMEWORK_DENSITY,
]

UnitCellRequest = dict[UnitCellKey, list[tuple[ComparisonOperator, float]]]


class CrystalInfoRequest(BaseModel):
    xrd_peak: list[XRDPeakRequest]
    unit_cell: UnitCellRequest


class TopoPropsRequest(BaseModel):
    scalars: dict[ScalarTopologicalPropertyKey, list[tuple[ComparisonOperator, float]]]
    composite_bu: list[str]
    secondary_bu: list[str]


class ZeoliteFrameworkReturnFields(BaseModel):
    crystal_info: list[CrystalInfoKey]
    topo_props: list[TopologicalPropertyKey]
    material: bool


class ZeoliteFrameworkRequest(BaseModel):
    crystal_info: CrystalInfoRequest
    topo_props: TopoPropsRequest


class CitationRequest(BaseModel):
    author_family_name: str | None
    year: int | None
    journal: str | None
    doi: str | None


class ZeoliticMaterialRequest(BaseModel):
    framework: str | None
    name: str | None
    formula: str | None
    framework_components: list[str]
    guest_components: list[str]
    unit_cell: UnitCellRequest
    citation: CitationRequest
