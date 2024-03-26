from enum import Enum
from typing import Union

from pydantic.dataclasses import dataclass


class FactoryIndustryKey(Enum):
    INDUSTRY = "Industry"


class FactoryNumAttrKey(Enum):
    DESIGN_CAPACITY = "DesignCapacity"
    SPECIFIC_ENERGY_CONSUMPTION = "SpecificEnergyConsumption"
    THERMAL_EFFICIENCY = "ThermalEfficiency"
    GENERATED_HEAT = "GeneratedHeat"


FactoryAttrKey = Union[FactoryIndustryKey, FactoryNumAttrKey]


class Industry(Enum):
    CHEMICAL = "ChemicalIndustry"
    FOOD = "FoodIndustry"
    SEMICONDUCTOR = "SemiconductorIndustry"
