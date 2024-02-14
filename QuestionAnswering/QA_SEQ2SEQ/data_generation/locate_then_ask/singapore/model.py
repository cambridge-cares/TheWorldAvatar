from dataclasses import dataclass
from decimal import Decimal
from typing import Optional

from locate_then_ask.model import OmMeasure


@dataclass(frozen=True)
class OPltPlot:
    land_use_type: Optional[str] # ozng:hasLandUseType(ONTOP)/a(BG)
    gross_plot_ratio: Optional[OmMeasure] # ^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue
    is_awaiting_detailed_gpr_eval: Optional[bool] # oplnrgl:isAwaitingDetailedGPREvaluation
    plot_area: Optional[OmMeasure] # oplt:hasPlotArea/om:hasValue
    gross_floor_area: Optional[OmMeasure] # oplt:hasMaximumPermittedGPR/om:hasValue
