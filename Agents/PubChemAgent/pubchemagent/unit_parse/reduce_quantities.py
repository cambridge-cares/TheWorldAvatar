from typing import Union, Any

from pubchemagent.unit_parse.config import Quantity, Unit
from pubchemagent.unit_parse.utils import quantity_difference
from pubchemagent.unit_parse.logger import log_debug, log_info
from pubchemagent.unit_parse.classification import QuantClass, quantity_classifier, ClassificationObj


@log_info
def reduce_quantities(data_in: Any,
                      order: tuple[QuantClass] = (QuantClass.SERIES_CONDITIONS, QuantClass.CONDITION,
                                                  QuantClass.SINGLE, QuantClass.NUMBER),
                      prefer_unit: Unit = None,
                      ) -> Quantity:
    """ reduce quantities

    First cleaning is done by units, then by data type.

    Unit ordering:
        1) preferred unit
        2) most common unit, Unless it is dimensionless than -->
            2a) take the most common non-dimensionless

    data type ordering:
        1) series condition (the largest series)
        2) conditions
        3) single (the highest repeated)
        4) single (middle value)
        5) number (the highest repeated)
        6) number (middle value)

    Parameters
    ----------
    data_in: Any
        data to be cleaned
    order: tuple[int]
        See QuantClass for integer values
    prefer_unit: Unit
        This unit will be preferred when down selecting

    Returns
    -------
    output: Any

    """
    # guard statements
    if not isinstance(data_in, list) or data_in == []:
        return data_in

    # generate data_dictionary
    class_data = quantity_classifier(data_in)

    # if data collapses to a single data_dict, then return early
    if len(class_data) == 1:
        return class_data[0].quantity

    # remove bad dimensionality
    class_data = remove_bad_dim(class_data, prefer_unit)

    # if data collapses to a single data_dict, then return early
    if len(class_data) == 1:
        return class_data[0].quantity

    # select base on ordering
    return _select_from_data_dict(class_data, order=order)


@log_debug
def _select_from_data_dict(class_data: list[ClassificationObj], order: tuple[QuantClass]) \
        -> Union[list[Quantity], Quantity]:
    """
    Select data based on order
    """
    for order_ in order:
        # check if QuantClass in dict in order given by 'order' parameter
        keys_of_hits = [data for data in class_data if order_ == data.type_]

        if len(keys_of_hits) == 0:  # no QuantClass in data
            continue

        elif len(keys_of_hits) == 1:  # one QuantClass found
            if order_ == QuantClass.SERIES_CONDITIONS or order_ == QuantClass.CONDITION:
                # return data_dict[keys_of_hits[0]]["quantity"]
                return double_check_output(class_data, keys_of_hits)

            return keys_of_hits[0].quantity

        else:  # two or more of a similar QuantClass found
            if order_ == QuantClass.SERIES_CONDITIONS:
                return double_check_output(class_data, keys_of_hits)

            elif order_ == QuantClass.CONDITION:
                return double_check_output(class_data, keys_of_hits)

            elif order_ == QuantClass.SINGLE:
                return _get_best_single(keys_of_hits)

            elif order_ == QuantClass.NUMBER:
                return _get_best_number(keys_of_hits)

    else:
        raise ValueError("Order parameter issue.")


def _get_best_single(class_data: list[ClassificationObj]):
    single = [[data.count, data.quantity] for data in class_data if data.type_ == QuantClass.SINGLE]

    # get one with most counts
    max_counts = max(single, key=lambda x: x[0])
    if max_counts[0] > 1:
        return max_counts[1]

    # get one in the middle
    return get_middle_quantity([i[1] for i in single])


def _get_best_number(class_data: list[ClassificationObj]):
    single = [[data.count, data.quantity] for data in class_data if data.type_ == QuantClass.NUMBER]

    # get one with most counts
    max_counts = max(single, key=lambda x: x[0])
    if max_counts[0] > 1:
        return max_counts[1]

    # get one in the middle
    return get_middle_quantity([i[1] for i in single])


@log_debug
def get_middle_quantity(data_in: list[Quantity]) -> Quantity:
    """ Remove data furthest from average till 1 point left."""
    data_in.sort()
    if len(data_in) % 2 == 0:
        index = int(len(data_in) / 2)
    else:
        index = int((len(data_in) - 1) / 2)

    return data_in[index]


def double_check_output(class_data: list[ClassificationObj], most_promising_key: list):
    """ double check output

    Check conditions, series and condition series against single (if there is a popular single).
    If unit dimensions don't match, take single.

    """
    # Check if there is some Single data to check against
    keys_of_hits = [data for data in class_data if data.type_ == QuantClass.SINGLE]
    counts = sum([data.count for data in keys_of_hits])
    if counts <= 2:  # if not more than two others, nothing to double-check
        return most_promising_key[0].quantity

    # get most common single unit
    single_quantity = _get_best_single(class_data)

    best_results = [1, single_quantity]
    for data in most_promising_key:
        smallest_diff = 1
        if data.type_ == QuantClass.SERIES_CONDITIONS:
            # look through SERIES_CONDITIONS for value closest to single_quantity
            for value in data.quantity:
                diff = quantity_difference(value[0], single_quantity)
                smallest_diff = diff if diff < smallest_diff else smallest_diff

        elif data.type_ == QuantClass.CONDITION:
            smallest_diff = quantity_difference(data.quantity[0][0], single_quantity)

        if smallest_diff < best_results[0]:
            best_results = [smallest_diff, data.quantity]

    return best_results[1]


@log_debug
def remove_bad_dim(data_class: list[ClassificationObj], prefer_unit: Unit = None) -> list[ClassificationObj]:
    """

    Remove data that doesn't match the most common dimension

    Parameters
    ----------
    data_class: list[ClassificationObj]

    prefer_unit: Unit
        Unit that will be preferred over all others. (If not found, choose most common).

    Returns
    -------

    """
    # remove Nones
    data_class = [data for data in data_class if data.unit is not None]

    dims = {}
    for data in data_class:
        dim = data.unit.dimensionality
        if dim in dims:  # if in dict, increase count
            dims[dim] += data.count
        else:
            dims[dim] = data.count

    most_common_dim = max(dims, key=dims.get)

    # prefer unit
    skip_flag = True
    if prefer_unit is not None:
        if most_common_dim != prefer_unit.dimensionality:
            most_common_dim = prefer_unit.dimensionality
            skip_flag = False

    # prefer non-dimensionless values first
    if skip_flag and most_common_dim == Unit("") and len(dims) > 1:
        dims.pop(most_common_dim)
        most_common_dim = max(dims, key=dims.get)

    # remove data that doesn't match most common dim
    return [data for data in data_class if data.unit.dimensionality == most_common_dim]
