from enum import Enum
from typing import Optional

from pydantic.dataclasses import dataclass

from model.constraint import ExtremeValueConstraint


class FactoryAttrKey(Enum):
    INDUSTRY = "Industry"  # ontocompany:belongsToIndustry/rdf:type
    GENERATED_HEAT = "GeneratedHeat"
    SPECIFIC_ENERGY_CONSUMPTION = "SpecificEnergyConsumption"
    THERMAL_EFFICIENCY = "ThermalEfficiency"
    DESIGN_CAPACITY = "DesignCapacity"


FACTORYATTR2UNIT = {
    FactoryAttrKey.GENERATED_HEAT: "MW",
    FactoryAttrKey.SPECIFIC_ENERGY_CONSUMPTION: "MJ/kg",
    FactoryAttrKey.THERMAL_EFFICIENCY: None,
    FactoryAttrKey.DESIGN_CAPACITY: "kg/s",
}


class Industry(Enum):
    CHEMICAL = "ChemicalIndustry"
    FOOD = "FoodIndustry"
    SEMICONDUCTOR = "SemiconductorIndustry"


@dataclass
class FactoryConstraints:
    industry: Optional[Industry] = None
    generated_heat: Optional[ExtremeValueConstraint] = None
    specific_energy_consumption: Optional[ExtremeValueConstraint] = None
    thermal_efficiency: Optional[ExtremeValueConstraint] = None
    design_capacity: Optional[ExtremeValueConstraint] = None

    def __str__(self):
        result = ""

        for key in [
            "industry",
            "generated_heat",
            "specific_energy_consumption",
            "thermal_efficiency",
            "design_capacity",
        ]:
            field = getattr(self, key)
            if field:
                result += "{key}: {value}".format(key=key, value=field.value)

        return result
