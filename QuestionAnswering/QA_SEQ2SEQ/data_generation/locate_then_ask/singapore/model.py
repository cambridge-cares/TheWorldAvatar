from dataclasses import dataclass
from functools import lru_cache
from typing import List, Optional
from constants.plot import OPltPlotAttrKey

from locate_then_ask.model import OmMeasure


@dataclass(frozen=True)
class OPltPlot:
    land_use_type_type: Optional[str]  # ozng:hasLandUseType(ONTOP)/a(BG)
    gross_plot_ratio: Optional[
        OmMeasure
    ]  # ^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue
    is_awaiting_detailed_gpr_eval: Optional[
        bool
    ]  # oplnrgl:isAwaitingDetailedGPREvaluation
    plot_area: Optional[OmMeasure]  # oplt:hasPlotArea/om:hasValue
    gross_floor_area: Optional[OmMeasure]  # oplt:hasMaximumPermittedGPR/om:hasValue

    @lru_cache
    def get_nonnone_keys(self):
        keys: List[OPltPlotAttrKey] = []
        if self.land_use_type_type is not None:
            keys.append(OPltPlotAttrKey.LAND_USE_TYPE_TYPE)
        if self.gross_plot_ratio is not None:
            keys.append(OPltPlotAttrKey.GROSS_PLOT_RATIO)
        if self.is_awaiting_detailed_gpr_eval is not None:
            keys.append(OPltPlotAttrKey.IS_AWAITING_DETAILED_GPR_EVAL)
        if self.plot_area is not None:
            keys.append(OPltPlotAttrKey.PLOT_AREA)
        if self.gross_floor_area is not None:
            keys.append(OPltPlotAttrKey.GROSS_FLOOR_AREA)
        return tuple(keys)
