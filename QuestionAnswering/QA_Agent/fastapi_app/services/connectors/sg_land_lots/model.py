from enum import Enum
from typing import Union


class PlotCatAttrKey(Enum):
    LAND_USE_TYPE = "LandUseType"


class PlotNumAttrKey(Enum):
    GROSS_PLOT_RATIO = "GrossPlotRatio"
    PLOT_AREA = "PlotArea"
    GROSS_FLOOR_AREA = "GrossFloorArea"


PlotAttrKey = Union[PlotCatAttrKey, PlotNumAttrKey]
