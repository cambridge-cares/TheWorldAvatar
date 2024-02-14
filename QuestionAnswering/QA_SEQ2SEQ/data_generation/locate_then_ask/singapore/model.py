from dataclasses import dataclass
from decimal import Decimal
from typing import Optional

from locate_then_ask.model import OmMeasure


@dataclass(frozen=True)
class OPltPlot:
    land_use_type: Optional[str]  # ozng:hasLandUseType(ONTOP)/a(BG)
    gross_plot_ratio: Optional[
        OmMeasure
    ]  # ^oplnrgl:appliesTo/oplnrgl:allowsGrossPlotRatio/om:hasValue
    is_awaiting_detailed_gpr_eval: Optional[
        bool
    ]  # oplnrgl:isAwaitingDetailedGPREvaluation
    plot_area: Optional[OmMeasure]  # oplt:hasPlotArea/om:hasValue
    gross_floor_area: Optional[OmMeasure]  # oplt:hasMaximumPermittedGPR/om:hasValue

    def get_nonnone_keys(self):
        return [
            k
            for k in [
                "land_use_type",
                "gross_plot_ratio",
                "is_awaiting_detailed_gpr_eval",
                "plot_area",
                "gross_floor_area",
            ]
            if getattr(self, k) is not None
        ]
