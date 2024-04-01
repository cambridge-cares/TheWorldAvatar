from enum import Enum
from typing import Optional, Union

from pydantic.dataclasses import dataclass


class PlotCatAttrKey(Enum):
    LAND_USE_TYPE = "LandUseType"


class PlotNumAttrKey(Enum):
    GROSS_PLOT_RATIO = "GrossPlotRatio"
    PLOT_AREA = "PlotArea"
    GROSS_FLOOR_AREA = "GrossFloorArea"


PlotAttrKey = Union[PlotCatAttrKey, PlotNumAttrKey]


@dataclass
class LandUseTypeNode:
    IRI: str
    label: str
    comment: str
    clsname: str
