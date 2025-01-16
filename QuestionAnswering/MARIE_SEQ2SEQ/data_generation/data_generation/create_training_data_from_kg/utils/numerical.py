
import random
from typing import Union


class FloatConversionError(ValueError):
    pass

def convert_to_float(number: str):
    try:
        return float(number)
    except ValueError as e:
        raise FloatConversionError(e.args)
    

def format_number(value: Union[int, float]):
    if isinstance(value, float):
        return "{:.2f}".format(value)
    return str(value)

def get_value_lower(value: str, eps=1e-6):
    value_out = convert_to_float(value)

    if value_out > 0:
        value_out *= 0.9
    else:
        value_out *= 1.1

    if abs(value_out) < eps:
        value_out = -0.5
    elif random.getrandbits(1):
        value_out = round(value_out)
        if abs(value_out - float(value)) < eps:
            value_out -= 1

    return format_number(value_out)

def get_value_higher(value: str, eps=1e-6):
    value_out = convert_to_float(value)

    if value_out > 0:
        value_out *= 1.1
    else:
        value_out *= 0.9

    if abs(value_out) < eps:
        value_out = 0.5
    elif random.getrandbits(1):
        value_out = round(value_out)
        if abs(value_out - float(value)) < eps:
            value_out += 1

    return format_number(value_out)

def get_value_around(value: str, eps=1e-6):
    value_out = convert_to_float(value)
    if random.getrandbits(1):
        value_out = round(value_out)
    return format_number(value_out)