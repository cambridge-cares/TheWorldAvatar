import pydantic

from chemistry_and_robots.data_model.iris import *

class DimensionalQuantity(pydantic.BaseModel):
    hasUnit: str
    hasNumericalValue: float

    def convert_to(self, new_unit: str):
        return unit_conversion_dq(self, new_unit)

class UnitConversionError(Exception):
    def __init__(self, current_unit: str, target_unit: str) -> None:
        full_message = "The unit conversion between unit <%s> and <%s> is NOT yet supported." % (current_unit, target_unit)
        super().__init__(full_message)

# TODO NOTE this information should be queried from KG of Units Of Measure if want to do properly
# TODO this should be developed as utils that also available to other agents
MAPPING_UNIT_CONVERSION_FACTOR = {
    OM_KILOGRAMPERMOLE:{OM_GRAMPERMOLE:1e3},
    OM_GRAMPERMOLE:{OM_KILOGRAMPERMOLE:1e-3},
    OM_LITRE:{OM_MILLILITRE:1e3},
    OM_MILLILITRE:{OM_LITRE:1e-3},
    OM_GRAMPERLITREPERHOUR:{OM_KILOGRAMPERLITREPERMINUTE:1/6e4},
    OM_KILOGRAMPERLITREPERMINUTE:{OM_GRAMPERLITREPERHOUR:6e4},
}
def unit_conversion_dq(dimensional_quantity: DimensionalQuantity, target_unit: str) -> DimensionalQuantity:
    if dimensional_quantity.hasUnit == target_unit: return dimensional_quantity
    try:
        return DimensionalQuantity(hasUnit=target_unit, hasNumericalValue=dimensional_quantity.hasNumericalValue * MAPPING_UNIT_CONVERSION_FACTOR[dimensional_quantity.hasUnit][target_unit])
    except KeyError:
        raise UnitConversionError(dimensional_quantity.hasUnit, target_unit)

def unit_conversion(value, current_unit: str, target_unit: str) -> DimensionalQuantity:
    if current_unit == target_unit: return DimensionalQuantity(hasUnit=target_unit, hasNumericalValue=value)
    try:
        return DimensionalQuantity(hasUnit=target_unit, hasNumericalValue=value * MAPPING_UNIT_CONVERSION_FACTOR[current_unit][target_unit])
    except KeyError:
        raise UnitConversionError(current_unit, target_unit)

def unit_conversion_return_value(value, current_unit: str, target_unit: str):
    if current_unit == target_unit: return value
    try:
        return value * MAPPING_UNIT_CONVERSION_FACTOR[current_unit][target_unit]
    except KeyError:
        raise UnitConversionError(current_unit, target_unit)

def unit_conversion_return_value_dq(dimensional_quantity: DimensionalQuantity, target_unit: str):
    if dimensional_quantity.hasUnit == target_unit: return dimensional_quantity.hasNumericalValue
    try:
        return dimensional_quantity.hasNumericalValue * MAPPING_UNIT_CONVERSION_FACTOR[dimensional_quantity.hasUnit][target_unit]
    except KeyError:
        raise UnitConversionError(dimensional_quantity.hasUnit, target_unit)
