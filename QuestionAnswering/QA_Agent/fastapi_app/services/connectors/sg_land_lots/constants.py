from collections import defaultdict
from functools import cache
from typing import Annotated, Dict, List

from fastapi import Depends

from services.connectors.sg_land_lots.kg import get_sgLandLots_bgClient
from services.core.kg import KgClient

from .model import PlotNumAttrKey


LAND_USE_TYPE_PREDICATE = "ontozoning:hasLandUseType"
PLOT_NUM_ATTR_PREDICATES = {
    PlotNumAttrKey.GROSS_PLOT_RATIO: "^opr:appliesTo/opr:allowsGrossPlotRatio/om:hasValue",
    PlotNumAttrKey.PLOT_AREA: "ontoplot:hasPlotArea/om:hasValue",
    PlotNumAttrKey.GROSS_FLOOR_AREA: "ontoplot:hasMaximumPermittedGPR/om:hasValue",
}

