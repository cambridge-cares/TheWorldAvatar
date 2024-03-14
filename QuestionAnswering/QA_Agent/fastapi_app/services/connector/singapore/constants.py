from enum import Enum
from importlib.resources import files


class PlotAttrKey(Enum):
    LAND_USE_TYPE = "LandUseType"
    GROSS_PLOT_RATIO = "GrossPlotRatio"
    PLOT_AREA = "PlotArea"
    GROSS_FLOOR_AREA = "GrossFloorArea"


LAND_USE_TYPES = (
    files("resources.cities.singapore_land_lots")
    .joinpath("land_use_type_iris.txt")
    .read_text()
    .split("/n")
)
