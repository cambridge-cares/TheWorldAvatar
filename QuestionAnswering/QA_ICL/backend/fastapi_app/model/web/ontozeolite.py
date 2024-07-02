from enum import Enum
from pydantic import BaseModel

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


class ScalarTopologicalPropertyKey(str, Enum):
    ACCESSIBLE_AREA_PER_CELL = "AccessibleAreaPerCell"
    ACCESSIBLE_AREA_PER_GRAM = "AccessibleAreaPerGram"
    ACCESSIBLE_VOLUME = "AccessibleVolume"
    ACCESSIBLE_VOLUME_PER_CELL = "AccessibleVolumePerCell"
    DENSITY = "Density"
    FRAMEWORK_DENSITY = "FrameworkDensity"
    OCCUPIABLE_AREA_PER_CELL = "OccupiableAreaPerCell"
    OCCUPIABLE_AREA_PER_GRAM = "OccupiableAreaPerGram"
    OCCUPIABLE_VOLUME = "OccupiableVolume"
    OCCUPIABLE_VOLUME_PER_CELL = "OccupiableVolumePerCell"
    SPECIFIC_ACCESSIBLE_AREA = "SpecificAccessibleArea"
    SPECIFIC_OCCUPIABLE_AREA = "SpecificOccupiableArea"
    TOPOLOGICAL_DENSITY = "TopologicalDensity"


class ZeoliteFrameworkRequest(BaseModel):
    xrd_peak: list[XRDPeakRequest]
    unit_cell: dict[UnitCellKey, list[tuple[ComparisonOperator, float]]]
    scalar_topological_properties: dict[
        ScalarTopologicalPropertyKey, list[tuple[ComparisonOperator, float]]
    ]
    composite_bu: list[str]
    secondary_bu: list[str]
