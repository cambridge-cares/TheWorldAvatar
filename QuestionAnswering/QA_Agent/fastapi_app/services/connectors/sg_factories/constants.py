from enum import Enum


class FactoryAttrKey(Enum):
    INDUSTRY = "Industry"
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


class FactoryConcept(Enum):
    CHEMICAL_PLANT = "http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant"
    FOOD_PLANT = "http://www.theworldavatar.com/kg/ontocompany#FoodPlant"
    SEMICONDUCTOR_PLANT = (
        "http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant"
    )
