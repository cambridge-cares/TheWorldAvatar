"""
Core

This code does the heavy lifting of turning strings into Quantities. Its the functions that need to deal with the
most mess.

"""
from typing import List, Union
import re

from pubchemagent.unit_parse.config import Unit, Quantity, config
from pubchemagent.unit_parse.pre_processing_substitution import sub_general
from pubchemagent.unit_parse.utils import flatten_list, contains_number, sig_figs, remove_empty_str, split_list
from pubchemagent.unit_parse.logger import log_debug, log_info, logger


@log_info
def text_list_to_quantity(text_list: Union[list[list[str]], list[str]]) -> list[Quantity]:
    """ text list to quantity

    Entry point for quantity parsing.
    It just loops through text_list and directs text to the quantity parsing.

    Parameters
    ----------
    text_list: list[list[str]], list[str]
        pre-parsed text list

    Returns
    -------
    Quantity: list[list[Quantity]], list[Quantity]

    """
    text_list = last_minute_sub(text_list)

    out = []
    for text in text_list:
        if len(text) == 1:
            result = get_quantity(text[0])
        else:
            result = get_quantity_and_cond(text)

        if result is not None:
            out.append(result)
    return out


@log_debug
def last_minute_sub(text_list: Union[list[list[str]], list[str]]) -> Union[list[list[str]], list[str]]:
    for i, obj in enumerate(text_list):
        if isinstance(obj, list):
            for ii, text in enumerate(obj):
                text_list[i][ii] = sub_general(text, config.last_minute_sub)
        else:
            text_list[i] = sub_general(obj, config.last_minute_sub)

    return text_list


@log_debug
def get_quantity_and_cond(text_list: list[str]) -> Union[Quantity, list[Quantity], None]:
    """ get quantity and condition

    Deals with a list[str] that should be a single quantity or two quantities.

    Parameters
    ----------
    text_list: list[str]

    Returns
    -------
    quantity: Quantity, list[Quantity], None

    """
    out = []
    for text in text_list:
        result = try_to_convert(text)
        if result is None:
            logger.warning(f"Text ignored: '{text}' in '{' '.join(text_list)}'")
        else:
            out.append(result)

    out = reduce_list(out)
    return out


def try_to_convert(text: str) -> Union[float, Unit, Quantity, None]:
    """ try to convert

    Try to turn string into a number, unit, quantity in that order. If all fails try complex parsing.

    Parameters
    ----------
    text: str

    Returns
    -------
    out: float, Unit, Quantity, None

    """
    try:
        return float(text)
    except Exception:
        pass
    try:
        return Unit(text)
    except Exception:
        pass
    try:
        return Quantity(text)
    except Exception:
        pass

    return get_quantity(text)


def reduce_list(obj_list: list[Union[str, int, float, Unit, Quantity]]) -> list[Union[str, Unit, Quantity]]:
    """

    Reduce a list of value, int, Unit, Quantity and "/" to a single or multiple quantities.

    Parameters
    ----------
    obj_list: list[Union[str, int, float, Unit, Quantity]]

    Returns
    -------
    out: list[Union[str, Unit, Quantity]]

    """
    # guard statement
    if len(obj_list) <= 1:
        return obj_list

    types_ = [type(obj) for obj in obj_list]
    out = [obj_list[0]]
    for t, obj in zip(types_[1:], obj_list[1:]):
        if type(out[-1]) in (int, float, Quantity) and t is Unit:
            out[-1] = out[-1] * obj
        else:
            out.append(obj)

    return out


@log_debug
def get_quantity(text_in: str) -> Union[Quantity, None]:
    """ get quantity

    Attempt to create Quantity from string.

    Parameters
    ----------
    text_in: str

    Returns
    -------
    quantity: quantity, None
        If unsuccessful return None

    """
    if not contains_number(text_in):
        return None

    try:  # let pint give it an attempt
        return Quantity(text_in)
    except Exception:  # if Pint can't do it try our custom method
        return to_quantity_expanded(text_in)


def to_quantity_expanded(text_in: str) -> Union[Quantity, None]:
    """ to quantity expanded

    Attempt to create Quantity from string stepwise process.
    Get value followed by get unit.

    Parameters
    ----------
    text_in: str

    Returns
    -------
    quantity: quantity, None
        If unsuccessful return None

    """
    value, text_value = get_value(text_in)
    if value is None:
        logger.warning(f"No value found: '{text_in}'")
        return None

    unit = get_unit(text_in.replace(text_value, ""))
    if unit is None:
        logger.warning(f"No unit found: '{text_in}' (value found: '{value})'")
        return None

    return value * unit


@log_debug
def get_value(text_in: str) -> tuple[Union[float, int, None], str]:
    """ get value

    Extracts value out of string. Value must be at the start of string.

    Parameters
    ----------
    text_in

    Returns
    -------
    value: float or int
        The value extracted
    value_text: str
        The text corresponding to the value.

    Examples
    --------
    "42.3 gcm-3" --> (42.3, '42.3')

    """
    try:
        result = re.findall('^[-]?[0-9.]+[*]?[0-9.]*[*]{0,2}[-]?[0-9.]*', text_in.lstrip())[0]
        return sig_figs(eval(result, {'__builtins__': None}), sig_digit=15), result
    except IndexError:
        return None, ""


@log_debug
def get_unit(text_in: str) -> Unit:
    """ get unit

    Attempts to turn string into unit

    Parameters
    ----------
    text_in

    Returns
    -------

    """
    if text_in is None or text_in == "":
        return None

    # pre-process unit chunk
    text_in = text_in.strip()
    split_text = split_on_powers(text_in)
    split_text = split_on_multiplication_symbol(split_text)
    split_text = split_on_division_symbol(split_text)

    # check if the pre-processing was enough to get a unit
    if all([isinstance(text, Unit) or text == "/" for text in split_text]):
        return merge_split_text(split_text)

    # dealing with messed up units
    split_text = split_list(split_text, " ", maxsplit=10)
    split_text = remove_empty_str(split_text)
    reduced_list = []
    for obj in split_text:
        if isinstance(obj, str) and obj != "/":
            result = frame_shift(obj)
            if result is None:
                logger.warning(f"Parsing unit warning: skipped text: '{obj}' in '{text_in}'")
            else:
                reduced_list.append(result)
        else:
            reduced_list.append(obj)

    return merge_split_text(reduced_list)


def split_on_powers(text_in: Union[str, list[str]]) -> List[Union[str, Unit]]:
    """

    Splits text up into a list of strings based on ** locations.

    Parameters
    ----------
    text_in

    Returns
    -------

    Examples
    --------
    "g**2cm**-3" --> [Unit("g**2"), Unit("cm**-3")]

    """
    if isinstance(text_in, str):
        text_in = [text_in]

    for i, text in enumerate(text_in):
        if isinstance(text, str) and "**" in text:
            out = re.split("([a-zA-Z]+[ ]?[*]{2}[ ]?[-+]?[0-9]+)", text, maxsplit=1)
            try:  # splits into 3 chunks, middle chunk may be a valid unit
                out[1] = Unit(out[1])
            except Exception:
                pass

            if "**" in out[2]:
                last_term = out.pop(2)
                out += split_on_powers(last_term)

            text_in[i] = out

    return remove_empty_str(flatten_list(text_in))


def split_on_multiplication_symbol(text_in: Union[str, list[str], list[Union[str, Unit]]]) -> \
        Union[List[Union[str, Unit]], None]:
    """

    Splits text up into a list of strings based on *

    Parameters
    ----------
    text_in

    Returns
    -------

    Examples
    --------
    "g*cm" --> [Unit("g"), Unit("cm")]

    """
    if isinstance(text_in, str):
        text_in = [text_in]

    for i, text in enumerate(text_in):
        if isinstance(text, str) and "*" in text:
            new_splits = re.split("([^*]+)[ ]?[*][ ]?([^*-0-9].*)", text)

            if len(new_splits) > 1:
                new_splits = [chunk for chunk in new_splits if chunk != ""]
                for ii, split in enumerate(new_splits):
                    try:
                        new_splits[ii] = Unit(split)
                        continue
                    except Exception:
                        pass
                    if bool(re.match("([^*]+)[ ]?[*][ ]?([^*-0-9].*)", split)):
                        new_splits[ii] = split_on_multiplication_symbol(split)  # pragma: no cover  recursive

                text_in[i] = new_splits
                continue
            else:
                if text[-1].strip() == "*":
                    if not bool(re.match(".*[a-zA-Z]+", text)):
                        return []

                    try:
                        text_in[i] = Unit(text[:-1])
                        continue
                    except Exception:
                        text_in[i] = text[:-1]

    return flatten_list(text_in)


def split_on_division_symbol(text_in: Union[str, list[str]]) -> List[str]:
    """

    Splits text up into a list of strings based on /

    Parameters
    ----------
    text_in

    Returns
    -------

    """
    if isinstance(text_in, str):
        text_in = [text_in]

    for i, text in enumerate(text_in):
        if isinstance(text, str) and "/" in text:
            new_splits = re.split("([/])", text)

            if len(new_splits) > 1:
                new_splits = [chunk for chunk in new_splits if chunk != ""]
                for ii, split in enumerate(new_splits):
                    try:
                        new_splits[ii] = Unit(split)
                        continue
                    except Exception:
                        pass

                text_in[i] = new_splits
                continue

    return remove_empty_str(flatten_list(text_in))


def merge_split_text(obj_list: List[Union[str, Unit]]) -> Union[Unit, None]:
    """

    Turns list[Unit] and "/" into a single Unit

    Parameters
    ----------
    obj_list

    Returns
    -------

    """
    unit: Unit = None
    buffer: Unit = None
    for obj in obj_list:
        if isinstance(obj, Unit):
            if buffer is None:
                buffer = obj
            else:  # do multiplication
                buffer = buffer * obj
        elif obj == "/":
            if unit is None:
                unit = buffer
            else:
                unit = unit / buffer
            buffer = None

    if buffer is not None:
        if unit is None:
            unit = buffer
        else:
            unit = unit / buffer

    return unit


@log_debug
def frame_shift(text_in: str) -> Unit:
    """

    Warning: "/" should not be in text
    """
    _frame_dict = {}
    for set_size in range(1, 9):
        for i in range(len(text_in)):
            upper_bound = i + set_size
            if upper_bound > len(text_in):
                break

            text = text_in[i: i + set_size]
            try:
                unit_ = Unit(text)
                _frame_dict[text] = {
                    "set_size": set_size,
                    "unit": unit_,
                    "bounds": [i, i + set_size]
                }
            except Exception:
                pass

    if _frame_dict == {}:
        return None

    replace = {}
    for i in range(10):
        # get max frame
        max_value = 0
        max_key = ""
        for k, v in _frame_dict.items():
            if v["set_size"] > max_value:
                max_value = v["set_size"]
                max_key = k

        replace[max_key] = _frame_dict.pop(max_key)

        remove_keys = []
        for k, v in _frame_dict.items():
            if replace[max_key]["bounds"][0] <= v["bounds"][0] < replace[max_key]["bounds"][1] or \
                    replace[max_key]["bounds"][0] < v["bounds"][1] <= replace[max_key]["bounds"][1]:
                remove_keys.append(k)

        for k in remove_keys:
            _frame_dict.pop(k)

        if not _frame_dict:
            break  # dictionary is empty

    # Taking "replace" and "text in" and merging
    count_list = list(range(0, len(text_in), 1))
    compile_list = []
    for i in range(0, len(text_in)):
        int_ = count_list[0]
        for v in replace.values():
            if v["bounds"][0] <= int_ < v["bounds"][1]:
                compile_list.append(v["unit"])
                remove_num = range(v["bounds"][0], v["bounds"][1])
                for num in remove_num:
                    count_list.remove(num)

        if not count_list:
            break

    else:
        return None

    out = compile_list[0]
    for i in compile_list[1:]:
        out = out * i

    return out
