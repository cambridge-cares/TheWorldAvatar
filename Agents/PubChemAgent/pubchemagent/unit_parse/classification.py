from typing import Union, Any
from enum import Enum
import math
import dataclasses

from pubchemagent.unit_parse.config import Quantity, Unit
from pubchemagent.unit_parse.utils import quantity_approx_equal, get_list_depth
from pubchemagent.unit_parse.logger import log_debug


class QuantClass(Enum):
    """ Classifications for data. """
    NUMBER = 0
    SINGLE = 1
    CONDITION = 2
    SERIES_CONDITIONS = 3


@dataclasses.dataclass
class ClassificationObj:
    quantity: Quantity
    unit: Unit
    count: int
    type_: QuantClass
    len_: int = None


@log_debug
def quantity_classifier(data_in: list[Any]) -> list[ClassificationObj]:
    """ quantity list to dict

    Generates a dict from list inside list, doing some classification.

    Parameters
    ----------
    data_in: list[Any]

    Returns
    -------
    output: list[ClassificationObj]

    """
    list_dim = get_list_depth(data_in)
    if list_dim == 0:
        return [dim_0_classifier(data_in)]
    elif list_dim == 1:
        return dim_1_classifier(data_in)
    elif list_dim == 2:
        return dim_2_classifier(data_in)
    elif list_dim == 3:
        return dim_3_classifier(data_in)
    else:
        raise ValueError("Can't handle list nesting of 3 or greater. ")


def dim_0_classifier(data_in: Union[Quantity, float, int]) -> ClassificationObj:
    if isinstance(data_in, (int, float)):
        return ClassificationObj(quantity=data_in, unit=Unit(""), count=1, type_=QuantClass.NUMBER)

    elif isinstance(data_in, Quantity):
        return ClassificationObj(quantity=data_in, unit=data_in.units, count=1, type_=QuantClass.SINGLE)

    else:
        raise TypeError(f"Invalid type (only, int, float, or Quantity): {data_in} ({type(data_in)})")


def dim_1_classifier(data_in: list[Any]) -> list[ClassificationObj]:
    out = []
    for i, data in enumerate(data_in):
        if isinstance(data, (float, int, Quantity)):
            temp = dim_0_classifier(data)
            out = _add_data(out, temp)
        else:
            raise TypeError(f"Invalid type (only, int, float, or Quantity): {data_in} ({type(data_in)})")

    return out


def _add_data(data: list[ClassificationObj], temp: ClassificationObj) -> list[ClassificationObj]:
    similar_values = [i for i, v in enumerate(data) if v.type_ == temp.type_]
    if len(similar_values) == 0:
        data.append(temp)
        return data

    if temp.type_ == QuantClass.NUMBER:
        for index in similar_values:
            if math.isclose(data[index].quantity, temp.quantity, rel_tol=0.02):
                data[index].count += temp.count
                return data
    elif temp.type_ == QuantClass.SINGLE:
        for index in similar_values:
            if quantity_approx_equal(data[index].quantity, temp.quantity, cutoff=0.02):
                data[index].count += temp.count
                return data

    data.append(temp)
    return data


def _add_data_list(data: list[ClassificationObj], temp: list[ClassificationObj]) -> list[ClassificationObj]:
    for i in temp:
        data = _add_data(data, i)

    return data


def dim_2_classifier(data_in: list[list[Any]]) -> list[ClassificationObj]:
    out = []
    cond = []
    for data in data_in:
        if not isinstance(data, list) or len(data) == 1:
            if isinstance(data, list):
                data = data[0]
            if isinstance(data, (float, int, Quantity)):
                out = _add_data(out, dim_0_classifier(data))
                continue
            else:
                raise TypeError(f"Invalid type (only, int, float, or Quantity): {data_in} ({type(data_in)})")

        if isinstance(data, list) and len(data) == 2:
            if not same_dimensionality(data[0], data[1]):
                cond.append(data)
                continue

        out = _add_data_list(out, dim_1_classifier(data))

    if len(cond) == 1:
        out.append(ClassificationObj(cond, unit=_get_dim(cond[0][0]), count=1, type_=QuantClass.CONDITION))
    elif len(cond) > 1:
        out.append(ClassificationObj(cond, unit=_get_dim([i[0] for i in cond]), count=1,
                                     type_=QuantClass.SERIES_CONDITIONS, len_=len(cond)))
    return out


def same_dimensionality(quantity1: Union[int, float, Quantity], quantity2: Union[int, float, Quantity]) -> bool:
    if isinstance(quantity1, (int, float)):
        quantity1 = Quantity(quantity1)
    if isinstance(quantity2, (int, float)):
        quantity2 = Quantity(quantity2)

    if not isinstance(quantity1, Quantity):
        raise TypeError(f"Invalid type (only, int, float, or Quantity): {quantity1} ({type(quantity1)})")
    if not isinstance(quantity2, Quantity):
        raise TypeError(f"Invalid type (only, int, float, or Quantity): {quantity2} ({type(quantity2)})")

    if quantity1.dimensionality == quantity2.dimensionality:
        return True
    return False


def dim_3_classifier(data_in: list[list[Any]]) -> list[ClassificationObj]:
    out = []
    for data in data_in:
        if not isinstance(data, list):
            if isinstance(data, (float, int, Quantity)):
                out = _add_data(out, dim_0_classifier(data))
                continue
            else:
                raise TypeError(f"Invalid type (only, int, float, or Quantity): {data_in} ({type(data_in)})")

        dim = get_list_depth(data)
        if dim == 1:
            out = _add_data_list(out, dim_1_classifier(data))
            continue

        out = _add_data_list(out, dim_2_classifier(data))

    return out


def _get_dim(obj: Union[int, float, Quantity]):
    if isinstance(obj, Quantity):
        return obj.units
    if isinstance(obj, list):
        unit_ = _get_dim(obj[0])
        for v in obj:
            unit__ = _get_dim(v)
            if unit_ != unit__:
                return None  # removes series later that don't have homogenous units
        else:
            return unit_

    return Unit("")
