from enum import Enum
from typing import Optional

from pydantic.dataclasses import dataclass

from model.constraint import ExtremeValueConstraint

class DataCentreAttrKey(Enum):
    GENERATED_HEAT = "GeneratedHeat"
    MAXIMUM_IT_CAPACITY = "MaximumITCapacity"
    UTILIZATION_RATE = "UtilizationRate"
    FLOOR_AREA = "FloorArea"


@dataclass
class DataCentreConstraints:
    generated_heat: Optional[ExtremeValueConstraint] = None
    maximum_it_capacity: Optional[ExtremeValueConstraint] = None
    utilization_rate: Optional[ExtremeValueConstraint] = None
    floor_area: Optional[ExtremeValueConstraint] = None